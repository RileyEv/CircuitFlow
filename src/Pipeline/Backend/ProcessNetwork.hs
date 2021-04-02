{-# LANGUAGE AllowAmbiguousTypes #-}
module Pipeline.Backend.ProcessNetwork (
    Network(..)
  , PipeList(..)
  , stopNetwork
  , input
  , output
  , taskExecuter
  , takeP
  , dropP
  , appendP
) where

import Pipeline.Core.DataStore (Apply, (:++))
import Pipeline.Core.HList (HList (..))
import Pipeline.Core.IFunctor (IFix4(..))
import Pipeline.Core.Nat (Take, Drop, SNat(..))
import Pipeline.Core.Task (TaskF)

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Chan (Chan, writeChan, readChan)

import Control.Monad (forM_, forever)

import Data.Type.Equality ((:~:)(..), gcastWith)
import Unsafe.Coerce (unsafeCoerce)

data PipeList (fs :: [* -> *]) (as :: [*]) (xs :: [*]) where
  PipeCons :: (Apply fs as ~ xs, Apply (f ': fs) (a ': as) ~ (f a ': xs))
    => Chan (f a) -> PipeList fs as xs -> PipeList (f ': fs) (a ': as) (f a ': xs)
  PipeNil :: PipeList '[] '[] (Apply '[] '[])


takeP :: (nS ~ nT) => SNat nS -> SNat nT -> PipeList fs as xs -> PipeList (Take nS fs) (Take nT as) (Take nS xs)
takeP SZero    SZero  _               = PipeNil
takeP (SSucc _) (SSucc _) PipeNil         = PipeNil
takeP (SSucc nS) (SSucc nT) (PipeCons x xs) = gcastWith (proof nS xs) $ PipeCons x (takeP nS nT xs)
  where
    proof :: SNat n -> PipeList fs as xs -> Apply (Take n fs) (Take n as) :~: Take n xs
    proof SZero PipeNil = Refl
    proof (SSucc n) (PipeCons x xs) = gcastWith (proof n xs) Refl



dropP :: (nS ~ nT) => SNat nS -> SNat nT -> PipeList fs as (Apply fs as) -> PipeList (Drop nS fs) (Drop nT as) (Drop nS (Apply fs as))
dropP SZero     SZero     l               = l
dropP (SSucc _) (SSucc _) PipeNil         = PipeNil
dropP (SSucc nS) (SSucc nT) (PipeCons _ xs) = dropP nS nT xs

appendP :: PipeList fs as (Apply fs as)
  -> PipeList gs bs (Apply gs bs)
  -> PipeList (fs :++ gs) (as :++ bs) ((Apply fs as) :++ (Apply gs bs))
appendP PipeNil ys = ys
appendP (PipeCons x xs) (PipeNil) = PipeCons x xs
appendP (PipeCons x xs) ys' = gcastWith (proof xs ys') $ PipeCons x (appendP xs ys')
  where
    proof :: PipeList fs' as' (Apply fs' as')
      -> PipeList gs' bs' (Apply gs' bs')
      -> Apply (fs' :++ gs') (as' :++ bs') :~: ((Apply fs' as') :++ (Apply gs' bs'))
    proof PipeNil ys = Refl
    proof xs'@(PipeCons x xs) ys = gcastWith (proof xs' ys) Refl





-- | Stores details about the network.
data Network (inputsStorage :: [* -> *]) (inputsType :: [*]) (outputsStorage :: [* -> *]) (outputsType :: [*]) where
  Network :: {
    threads :: [ThreadId],
    inputs :: PipeList inputsStorage inputsType (Apply inputsStorage inputsType),
    outputs :: PipeList outputsStorage outputsType (Apply outputsStorage outputsType) }
    -> Network inputsStorage inputsType outputsStorage outputsType



{-|
Stops the given network
-}
stopNetwork :: Network inputS inputsT outputsS outputsT -> IO ()
stopNetwork n = forM_ (threads n) killThread


taskExecuter :: IFix4 TaskF inputsS inputsT outputS outputT
  -> PipeList inputsS inputsT (Apply inputsS inputsT)
  -> PipeList outputS outputT (Apply outputS outputT)
  -> IO ()
taskExecuter t ins out = forever (do
  return ())


input :: HList (Apply inputsS inputsT) -> Network inputsS inputsT outputsS outputsT -> IO ()
input xs n = input' xs (inputs n)
  where
    input' :: HList (Apply inputsS inputsT) -> PipeList inputsS inputsT (Apply inputsS inputsT) -> IO ()
    input' HNil PipeNil = return ()
    input' (HCons x xs) (PipeCons p ps) = writeChan p x >> input' xs ps

output :: Network inputsS inputsT outputsS outputsT -> IO (HList (Apply outputsS outputsT))
output n = output' (outputs n)
  where
    output' :: PipeList outputsS outputsT (Apply outputsS outputsT) -> IO (HList (Apply outputsS outputsT))
    output' PipeNil = return HNil
    output' (PipeCons p ps) = readChan p >>= \x -> output' ps >>= \xs -> return (HCons x xs)
