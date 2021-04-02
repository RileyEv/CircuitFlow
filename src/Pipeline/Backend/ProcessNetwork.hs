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
  , AppendP
) where

import Pipeline.Core.DataStore (Apply, (:++))
import Pipeline.Core.HList (HList (..))
import Pipeline.Core.IFunctor (IFix6(..))
import Pipeline.Core.Nat (Take, Drop, SNat(..))
import Pipeline.Core.Task (TaskF)

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Chan (Chan, writeChan, readChan)

import Control.Monad (forM_, forever)

import Data.Type.Equality ((:~:)(..), gcastWith)
import Unsafe.Coerce (unsafeCoerce)

data PipeList (fs :: [* -> *]) (as :: [*]) (xs :: [*]) where
  PipeCons :: Chan (f a) -> PipeList fs as xs -> PipeList (f ': fs) (a ': as) (f a ': xs)
  PipeNil :: PipeList '[] '[] (Apply '[] '[])


takeP :: SNat n -> PipeList fs as xs -> PipeList (Take n fs) (Take n as) (Take n xs)
takeP SZero     _               = PipeNil
takeP (SSucc _) PipeNil         = PipeNil
takeP (SSucc n) (PipeCons x xs) = PipeCons x (takeP n xs)


dropP :: SNat n -> PipeList fs as xs -> PipeList (Drop n fs) (Drop n as) (Drop n xs)
dropP SZero      l               = l
dropP (SSucc _)  PipeNil         = PipeNil
dropP (SSucc n) (PipeCons _ xs) = dropP n xs


class AppendP fs as xs gs bs ys where
  appendP :: PipeList fs as xs -> PipeList gs bs ys -> PipeList (fs :++ gs) (as :++ bs) (xs :++ ys)

instance AppendP '[] '[] '[] gs bs ys where
  appendP PipeNil ys = ys

instance (AppendP fs as xs gs bs ys) => AppendP (f ': fs) (a ': as) (f a ': xs) gs bs ys where
  appendP (PipeCons x xs) ys = PipeCons x (appendP xs ys)
  
-- appendP (PipeCons x xs) (PipeNil) = PipeCons x xs
-- appendP (PipeCons x xs) ys' = gcastWith (proof xs ys') $ PipeCons x (appendP xs ys')
--   where
--     proof :: PipeList fs' as' (Apply fs' as')
--       -> PipeList gs' bs' (Apply gs' bs')
--       -> Apply (fs' :++ gs') (as' :++ bs') :~: ((Apply fs' as') :++ (Apply gs' bs'))
--     proof PipeNil ys = Refl
--     proof xs'@(PipeCons x xs) ys = gcastWith (proof xs' ys) Refl





-- | Stores details about the network.
data Network (inputsStorage :: [* -> *]) (inputsType :: [*]) (inputsAp :: [*]) (outputsStorage :: [* -> *]) (outputsType :: [*]) (outputsAp :: [*]) where
  Network :: {
    threads :: [ThreadId],
    inputs :: PipeList inputsStorage inputsType inputsAp,
    outputs :: PipeList outputsStorage outputsType outputsAp }
    -> Network inputsStorage inputsType inputsAp outputsStorage outputsType outputsAp



{-|
Stops the given network
-}
stopNetwork :: Network inputS inputsT inputsA outputsS outputsT outputsA -> IO ()
stopNetwork n = forM_ (threads n) killThread


taskExecuter :: IFix6 TaskF inputsS inputsT inputsA outputS outputT outputsA
  -> PipeList inputsS inputsT inputsA
  -> PipeList outputS outputT outputA
  -> IO ()
taskExecuter t ins out = forever (do
  return ())


input :: HList inputsA -> Network inputsS inputsT inputsA outputsS outputsT outputsA -> IO ()
input xs n = input' xs (inputs n)
  where
    input' :: HList inputsA -> PipeList inputsS inputsT inputsA -> IO ()
    input' HNil PipeNil = return ()
    input' (HCons x xs) (PipeCons p ps) = writeChan p x >> input' xs ps

output :: Network inputsS inputsT inputsA outputsS outputsT outputsA -> IO (HList outputsA)
output n = output' (outputs n)
  where
    output' :: PipeList outputsS outputsT outputsA -> IO (HList outputsA)
    output' PipeNil = return HNil
    output' (PipeCons p ps) = readChan p >>= \x -> output' ps >>= \xs -> return (HCons x xs)
