module Pipeline.Backend.ProcessNetwork (
    Network
  , startNetwork
  , stopNetwork
  , input
  , output
) where

import Pipeline.Core.HList (HList (..))
import Pipeline.Core.IFunctor (IFix2(..))
import Pipeline.Core.PID (PID)
import Pipeline.Core.Task (TaskF)

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Chan (Chan, writeChan, readChan)

import Control.Monad (forM_, forever)

data ComputationNode where
  ComputationNode :: forall (fas :: [*]) (gb :: [*]). IFix2 TaskF fas gb -> ComputationNode

data Pipe = forall a. Pipe PID PID a

data PipeList (xs :: [*]) where
  PipeCons :: Chan a -> PipeList xs -> PipeList (a ': xs)
  PipeNil :: PipeList '[]


-- | Stores details about the network.
data Network inputs outputs where
  Network :: { threads :: [ThreadId], inputs :: PipeList inputs, outputs :: PipeList outputs } -> Network inputs outputs


{-|
Starts the network

Returns the input pipes and output pipes.
-}
startNetwork :: [ComputationNode] -> [Pipe] -> IO (Network inputs outputs)
startNetwork = undefined

{-|
Stops the given network
-}
stopNetwork :: Network input outputs -> IO ()
stopNetwork n = forM_ (threads n) killThread


taskExecuter :: IFix2 TaskF inputs output -> PipeList inputs -> PipeList output -> IO ()
taskExecuter t ins out = forever (do
  return ())


input :: HList inputs -> Network inputs outputs -> IO ()
input xs n = input' xs (inputs n)
  where
    input' :: HList inputs -> PipeList inputs -> IO ()
    input' HNil PipeNil = return ()
    input' (HCons x xs) (PipeCons p ps) = writeChan p x >> input' xs ps

output :: Network inputs outputs -> IO (HList outputs)
output n = output' (outputs n)
  where
    output' :: PipeList outputs -> IO (HList outputs)
    output' PipeNil = return HNil
    output' (PipeCons p ps) = readChan p >>= \x -> output' ps >>= \xs -> return (HCons x xs)
