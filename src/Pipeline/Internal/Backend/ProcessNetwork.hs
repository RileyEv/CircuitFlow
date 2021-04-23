{-# LANGUAGE AllowAmbiguousTypes #-}
module Pipeline.Internal.Backend.ProcessNetwork
  ( Network(..)
  , stopNetwork
  , inputUUID
  , output
  , taskExecuter
  ) where

import           Control.Concurrent                (ThreadId, killThread)
import           Control.Concurrent.Chan           (readChan, writeChan)
import           Control.DeepSeq                   (NFData, deepseq)
import           Control.Exception                 (SomeException,
                                                    displayException)
import           Control.Exception.Lifted          (try)
import           Control.Monad                     (forM_, forever)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans               (lift)
import           Control.Monad.Trans.Except        (ExceptT (..), catchE,
                                                    runExceptT, throwE)
import           Data.Kind                         (Type)
import           GHC.Generics                      (Generic)
import           Pipeline.Internal.Common.HList    (HList' (..))
import           Pipeline.Internal.Core.CircuitAST (Task (..))
import           Pipeline.Internal.Core.Error      (ExceptionMessage (..),
                                                    TaskError (..))
import           Pipeline.Internal.Core.PipeList   (PipeList (..))
import           Pipeline.Internal.Core.UUID       (UUID)
import           Prelude                           hiding (read)
import Pipeline.Internal.Common.Nat (Nat)

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

data Network' (inputsStorageType  :: [Type -> Type]) (inputsType  :: [Type]) (inputsAp  :: [Type])
              (outputsStorageType :: [Type -> Type]) (outputsType :: [Type]) (outputsAp :: [Type])
              (ninputs :: Nat) where
  Network' ::{
    threads' :: [ThreadId],
    inputs' :: PipeList inputsStorage inputsType inputsAp,
    outputs' :: PipeList outputsStorage outputsType outputsAp }
    -> Network' inputsStorage inputsType inputsAp outputsStorage outputsType outputsAp ninputs

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
      (runExceptT
        (do
          input <- (ExceptT . return) taskInputs
          r     <- catchE (intercept (f uuid input outStore))
                          (throwE . TaskError . ExceptionMessage . displayException)
          return (HCons' (r `deepseq` r) HNil')
        )
      )
    write uuid r outPipes
  )


intercept :: ExceptT SomeException IO a -> ExceptT SomeException IO a
intercept a = do
  r <- try a
  case r of
    Right x -> return x
    Left  e -> throwE e


write
  :: UUID -> Either TaskError (HList' inputsS inputsT) -> PipeList inputsS inputsT inputsA -> IO ()
write _    (Left  e    ) PipeNil         = return ()
write uuid (Left  e    ) (PipeCons p ps) = writeChan p (uuid, Left e) >> write uuid (Left e) ps
write _    (Right HNil') PipeNil         = return ()
write uuid (Right (HCons' x xs)) (PipeCons p ps) =
  writeChan p (uuid, Right x) >> write uuid (Right xs) ps

read
  :: PipeList outputsS outputsT outputsA -> IO (UUID, Either TaskError (HList' outputsS outputsT))
read PipeNil         = return ("", Right HNil')
read (PipeCons p ps) = do
  (uuid, x ) <- readChan p
  (_   , xs) <- read ps
  case x of
    Right x' -> case xs of
      Right xs' -> return (uuid, Right (HCons' x' xs'))
      Left  e   -> return (uuid, Left e)
    Left e -> return (uuid, Left e)

-- | A variant of 'input', with a user specified unique identifier.
inputUUID
  :: UUID -- ^ A unique identifier for the input values
  -> HList' inputsS inputsT -- ^ The input values
  -> Network inputsS inputsT inputsA outputsS outputsT outputsA -- ^ The network to input the values in to
  -> IO ()
inputUUID uuid xs n = write uuid (Right xs) (inputs n)

-- | This will read from the outputs of the network.
--
--   /This is a blocking call, therefore if there are no outputs to be read then the program will deadlock./
output
  :: Network inputsS inputsT inputsA outputsS outputsT outputsA -- ^ The network to retrieve inputs from
  -> IO (UUID, Either TaskError (HList' outputsS outputsT)) -- ^ The identifier for the output and the output values
output n = read (outputs n)
