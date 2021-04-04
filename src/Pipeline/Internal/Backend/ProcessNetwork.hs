{-# LANGUAGE AllowAmbiguousTypes #-}
module Pipeline.Internal.Backend.ProcessNetwork (
  Network(..),
  stopNetwork,
  input,
  output,
  taskExecuter,
) where

import Prelude hiding (read)

import Pipeline.Internal.Core.CircuitAST (Task(..))
import Pipeline.Internal.Core.PipeList (PipeList(..))
import Pipeline.Internal.Common.HList (HList'(..))

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Chan (writeChan, readChan)

import Control.Monad (forM_, forever)

import Data.Kind (Type)


-- | Main type for storing information about the process network.
-- 
--   The type arguments align with the 'Pipeline.Circuit.Circuit' data type.
data Network (inputsStorageType  :: [Type -> Type]) (inputsType  :: [Type]) (inputsAp  :: [Type])
             (outputsStorageType :: [Type -> Type]) (outputsType :: [Type]) (outputsAp :: [Type]) where
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


taskExecuter :: Task iF inputsS inputsT inputsA outputS outputT outputsA ninputs
  -> PipeList inputsS inputsT inputsA
  -> PipeList outputS outputT outputA
  -> IO ()
taskExecuter (Task f outStore) inPipes outPipes = forever (do
  taskInputs <- read inPipes
  r <- f taskInputs outStore
  write (HCons' r HNil') outPipes)
 

write :: HList' inputsS inputsT -> PipeList inputsS inputsT inputsA -> IO ()
write HNil' PipeNil = return ()
write (HCons' x xs) (PipeCons p ps) = writeChan p x >> write xs ps

read :: PipeList outputsS outputsT outputsA -> IO (HList' outputsS outputsT)
read PipeNil = return HNil'
read (PipeCons p ps) = readChan p >>= \x -> read ps >>= \xs -> return (HCons' x xs)

-- | This will write the given input to the network 
input :: HList' inputsS inputsT -> Network inputsS inputsT inputsA outputsS outputsT outputsA -> IO ()
input xs n = write xs (inputs n)

-- | This will read from the outputs of the network.
--
--   This is a blocking call, therefore if there are no outputs to be read then the program will deadlock.
output :: Network inputsS inputsT inputsA outputsS outputsT outputsA -> IO (HList' outputsS outputsT)
output n = read (outputs n)
