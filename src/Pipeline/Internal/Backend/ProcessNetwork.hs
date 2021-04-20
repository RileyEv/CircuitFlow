{-# LANGUAGE AllowAmbiguousTypes #-}
module Pipeline.Internal.Backend.ProcessNetwork
  ( Network(..)
  , stopNetwork
  , inputUUID
  , output
  , taskExecuter
  ) where

import           Prelude                           hiding (read)

import           Pipeline.Internal.Common.HList    (HList' (..))
import           Pipeline.Internal.Core.CircuitAST (Task (..))
import           Pipeline.Internal.Core.PipeList   (PipeList (..))
import           Pipeline.Internal.Core.UUID       (UUID)

import           Control.Concurrent                (ThreadId, killThread)
import           Control.Concurrent.Chan           (readChan, writeChan)

import           Control.Monad                     (forM_, forever, void)
import           Control.Monad.Trans               (lift)
import           Control.Monad.Trans.Maybe         (MaybeT (..), runMaybeT)

import           Data.Kind                         (Type)


-- | Main type for storing information about the process network.
--
--   The type arguments align with the 'Pipeline.Circuit.Circuit' data type.
data Network (inputsStorageType  :: [Type -> Type]) (inputsType  :: [Type]) (inputsAp  :: [Type])
             (outputsStorageType :: [Type -> Type]) (outputsType :: [Type]) (outputsAp :: [Type]) where
  Network ::{
    threads :: [ThreadId],
    inputs :: PipeList inputsStorage inputsType inputsAp,
    outputs :: PipeList outputsStorage outputsType outputsAp }
    -> Network inputsStorage inputsType inputsAp outputsStorage outputsType outputsAp


{-|
Stops the given network
-}
stopNetwork :: Network inputS inputsT inputsA outputsS outputsT outputsA -> IO ()
stopNetwork n = forM_ (threads n) killThread


taskExecuter
  :: Task iF inputsS inputsT inputsA outputS outputT outputsA ninputs
  -> PipeList inputsS inputsT inputsA
  -> PipeList outputS outputT outputA
  -> IO ()
taskExecuter (Task f outStore) inPipes outPipes = forever
  (do
    (uuid, taskInputs) <- read inPipes
    r                  <-
      (runMaybeT
        (do
          input <- (MaybeT . return) taskInputs
          r     <- f uuid input outStore
          return (HCons' r HNil')
        )
      )
    write uuid r outPipes
  )


write :: UUID -> Maybe (HList' inputsS inputsT) -> PipeList inputsS inputsT inputsA -> IO ()
write _    Nothing      PipeNil         = return ()
write uuid Nothing      (PipeCons p ps) = writeChan p (uuid, Nothing) >> write uuid Nothing ps
write _    (Just HNil') PipeNil         = return ()
write uuid (Just (HCons' x xs)) (PipeCons p ps) =
  writeChan p (uuid, Just x) >> write uuid (Just xs) ps

read :: PipeList outputsS outputsT outputsA -> IO (UUID, Maybe (HList' outputsS outputsT))
read PipeNil         = return ("", Just HNil')
read (PipeCons p ps) = do
  (uuid, x ) <- readChan p
  (_   , xs) <- read ps
  case x of
    Just x' -> case xs of
      Just xs' -> return (uuid, Just (HCons' x' xs'))
      Nothing  -> return (uuid, Nothing)
    Nothing -> return (uuid, Nothing)

-- | A variant of 'input', with a user specified unique identifier.
inputUUID
  :: UUID -- ^ A unique identifier for the input values
  -> HList' inputsS inputsT -- ^ The input values
  -> Network inputsS inputsT inputsA outputsS outputsT outputsA -- ^ The network to input the values in to
  -> IO ()
inputUUID uuid xs n = write uuid (Just xs) (inputs n)

-- | This will read from the outputs of the network.
--
--   /This is a blocking call, therefore if there are no outputs to be read then the program will deadlock./
output
  :: Network inputsS inputsT inputsA outputsS outputsT outputsA -- ^ The network to retrieve inputs from
  -> IO (UUID, Maybe (HList' outputsS outputsT)) -- ^ The identifier for the output and the output values
output n = read (outputs n)
