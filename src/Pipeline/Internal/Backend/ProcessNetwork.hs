{-# LANGUAGE AllowAmbiguousTypes #-}
module Pipeline.Internal.Backend.ProcessNetwork (
  Network(..),
  stopNetwork,
  inputUUID,
  output,
  taskExecuter,
) where

import Prelude hiding (read)

import Pipeline.Internal.Core.CircuitAST (Task(..))
import Pipeline.Internal.Core.PipeList (PipeList(..))
import Pipeline.Internal.Core.UUID (UUID)
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
  (uuid, taskInputs) <- read inPipes
  r <- f uuid taskInputs outStore
  write uuid (HCons' r HNil') outPipes)
 

write :: UUID -> HList' inputsS inputsT -> PipeList inputsS inputsT inputsA -> IO ()
write _ HNil' PipeNil = return ()
write uuid (HCons' x xs) (PipeCons p ps) = writeChan p (uuid, x) >> write uuid xs ps

read :: PipeList outputsS outputsT outputsA -> IO (UUID, HList' outputsS outputsT)
read PipeNil = return ("", HNil')
read (PipeCons p ps) = readChan p >>= \(uuid, x) -> read ps >>= \(_, xs) -> return (uuid, HCons' x xs)

-- | A variant of 'input', with a user specified unique identifier.
inputUUID :: UUID -- ^ A unique identifier for the input values
  -> HList' inputsS inputsT -- ^ The input values
  -> Network inputsS inputsT inputsA outputsS outputsT outputsA -- ^ The network to input the values in to
  -> IO ()
inputUUID uuid xs n = write uuid xs (inputs n)

-- | This will read from the outputs of the network.
--
--   /This is a blocking call, therefore if there are no outputs to be read then the program will deadlock./
output :: Network inputsS inputsT inputsA outputsS outputsT outputsA -- ^ The network to retrieve inputs from
  -> IO (UUID, HList' outputsS outputsT) -- ^ The identifier for the output and the output values
output n = read (outputs n)
