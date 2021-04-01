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

import Pipeline.Core.DataStore (Apply, type (++))
import Pipeline.Core.HList (HList (..))
import Pipeline.Core.IFunctor (IFix4(..))
import Pipeline.Core.Nat (Take, Drop, SNat(..))
import Pipeline.Core.Task (TaskF)

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Chan (Chan, writeChan, readChan)

import Control.Monad (forM_, forever)

import Data.Type.Equality ((:~:)(..), gcastWith)
import Unsafe.Coerce (unsafeCoerce)

data PipeList (fs :: [* -> *]) (as :: [*]) where
  PipeCons :: Chan (f a) -> PipeList fs as -> PipeList (f ': fs) (a ': as)
  PipeNil :: PipeList '[] '[]


takeP :: SNat nS -> SNat nT -> PipeList fs as -> PipeList (Take nS fs) (Take nT as)
takeP SZero    SZero  _               = PipeNil
takeP (SSucc _) (SSucc _) PipeNil         = PipeNil
takeP (SSucc nS) (SSucc nT) (PipeCons x xs) = PipeCons x (takeP nS nT xs)

dropP :: SNat nS -> SNat nT -> PipeList fs as -> PipeList (Drop nS fs) (Drop nT as)
dropP SZero     SZero     l               = l
dropP (SSucc _) (SSucc _) PipeNil         = PipeNil
dropP (SSucc nS) (SSucc nT) (PipeCons _ xs) = dropP nS nT xs

appendP :: PipeList fs as -> PipeList gs bs -> PipeList (fs ++ gs) (as ++ bs)
appendP PipeNil ys = ys
appendP (PipeCons x xs) ys = gcastWith (proof2 x xs ys) (gcastWith (proof1 x xs ys) $ PipeCons x (appendP xs ys))
  where
    proof1 :: Chan (f a) -> PipeList fs as -> PipeList gs bs -> ((f ': fs) ++ gs) :~: (f ': (fs ++ gs))
    proof1 c PipeNil ys = Refl
    proof1 c (PipeCons x xs) ys = unsafeCoerce Refl
    proof2 :: Chan (f a) -> PipeList fs as -> PipeList gs bs -> ((a ': as) ++ bs) :~: (a ': (as ++ bs))
    proof2 c PipeNil ys = Refl
    proof2 c (PipeCons x xs) ys = unsafeCoerce Refl




-- | Stores details about the network.
data Network (inputsStorage :: [* -> *]) (inputsType :: [*]) (outputsStorage :: [* -> *]) (outputsType :: [*]) where
  Network :: {
    threads :: [ThreadId],
    inputs :: PipeList inputsStorage inputsTypes,
    outputs :: PipeList outputsStorage outputsType } -> Network inputsStorage inputsTypes outputsStorage outputsType



{-|
Stops the given network
-}
stopNetwork :: Network inputS inputsT outputsS outputsT -> IO ()
stopNetwork n = forM_ (threads n) killThread


taskExecuter :: IFix4 TaskF inputsS inputsT outputS outputT -> PipeList inputsS inputsT -> PipeList outputS outputT -> IO ()
taskExecuter t ins out = forever (do
  return ())


input :: HList (Apply inputsS inputsT) -> Network inputsS inputsT outputsS outputsT -> IO ()
input xs n = input' xs (inputs n)
  where
    input' :: HList (Apply inputsS inputsT) -> PipeList inputsS inputsT -> IO ()
    input' HNil PipeNil = return ()
    input' (HCons x xs) (PipeCons p ps) = writeChan p x >> input' xs ps

output :: Network inputsS inputsT outputsS outputsT -> IO (HList (Apply outputsS outputsT))
output n = output' (outputs n)
  where
    output' :: PipeList outputsS outputsT -> IO (HList (Apply outputsS outputsT))
    output' PipeNil = return HNil
    output' (PipeCons p ps) = readChan p >>= \x -> output' ps >>= \xs -> return (HCons x xs)
