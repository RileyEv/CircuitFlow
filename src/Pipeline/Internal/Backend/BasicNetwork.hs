{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pipeline.Internal.Backend.BasicNetwork where

import           Control.Concurrent                (ThreadId, forkIO,
                                                    killThread)
import           Control.Concurrent.Chan           (dupChan, newChan, readChan,
                                                    writeChan)
import           Control.Exception                 (SomeException,
                                                    displayException)
import           Control.Exception.Lifted          (try)
import           Control.Monad                     (forM_, forever, (>=>))
import           Control.Monad.Trans.Except        (ExceptT (..), catchE,
                                                    runExceptT, throwE)
import           Data.Kind                         (Type)
import qualified Data.Map as M                     (Map, empty, insert, union)
import           Control.Monad.Trans               (lift)
import           Pipeline.Internal.Backend.Network (BuildNetworkAlg (..),
                                                    InitialPipes (..), N (..),
                                                    Network (..))
import           Pipeline.Internal.Common.HList    (HList' (..), HList(..))
import           Pipeline.Internal.Common.IFunctor (icataM5)
import           Pipeline.Internal.Common.Nat      (IsNat (..), Nat,
                                                    SNat (..), N1)
import           Pipeline.Internal.Common.TypeList (Drop, Length, Take,
                                                    (:++))
import           Pipeline.Internal.Core.CircuitAST
import           Pipeline.Internal.Core.Error      (ExceptionMessage (..),
                                                    TaskError (..))
import           Pipeline.Internal.Core.PipeList   (AppendP (..), PipeList (..),
                                                    dropP, takeP)
import           Pipeline.Internal.Core.UUID       (JobUUID, nilJobUUID, TaskUUID, genUnusedTaskUUID)
import           Pipeline.Internal.Core.DataStore  (Var, emptyVar, DataStore'(..), DataStore(..))
import           Prelude                           hiding (read)

-- | Main type for storing information about the process network.
--
--   The type arguments align with the 'Pipeline.Circuit.Circuit' data type.
data BasicNetwork (inputsStorageType  :: [Type -> Type]) (inputsType  :: [Type])
             (outputsStorageType :: [Type -> Type]) (outputsType :: [Type]) where
  BasicNetwork ::{
    threads :: M.Map TaskUUID ThreadId,
    jobs :: M.Map JobUUID JobStatus,
    inputs :: PipeList inputsStorage inputsType,
    outputs :: PipeList outputsStorage outputsType }
    -> BasicNetwork inputsStorage inputsType outputsStorage outputsType

data JobStatus = Processing | Complete

instance Network BasicNetwork where
  startNetwork = buildBasicNetwork
  stopNetwork n = forM_ (threads n) killThread
  write uuid xs n = writePipes uuid (Right xs) (inputs n)
  read n = readPipes (outputs n)

-- Task Execution
taskExecuter
  :: Task iF inputsS inputsT outputS outputT ninputs
  -> TaskUUID
  -> PipeList inputsS inputsT
  -> PipeList outputS outputT
  -> IO ()
taskExecuter (Task f) taskUUID inPipes outPipes = forever
  (do
    (jobUUID, taskInputs) <- readPipes inPipes
    r                  <-
      runExceptT
        (do
          outputStore <- lift (empty taskUUID jobUUID)
          input <- (ExceptT . return) taskInputs
          catchE (intercept (f input outputStore))
                 (throwE . TaskError . ExceptionMessage . displayException)
          return (HCons' outputStore HNil')
        )

    writePipes jobUUID r outPipes
  )


intercept :: ExceptT SomeException IO a -> ExceptT SomeException IO a
intercept a = do
  r <- try a
  case r of
    Right x -> return x
    Left  e -> throwE e


-- Network IO

writePipes
  :: JobUUID -> Either TaskError (HList' inputsS inputsT) -> PipeList inputsS inputsT -> IO ()
writePipes _ (Left _) PipeNil = return ()
writePipes uuid (Left e) (PipeCons p ps) =
  writeChan p (uuid, Left e) >> writePipes uuid (Left e) ps
writePipes _ (Right HNil') PipeNil = return ()
writePipes uuid (Right (HCons' x xs)) (PipeCons p ps) =
  writeChan p (uuid, Right x) >> writePipes uuid (Right xs) ps

readPipes
  :: PipeList outputsS outputsT -> IO (JobUUID, Either TaskError (HList' outputsS outputsT))
readPipes PipeNil         = return (nilJobUUID, Right HNil')
readPipes (PipeCons p ps) = do
  (uuid, x ) <- readChan p
  (_   , xs) <- readPipes ps
  case x of
    Right x' -> case xs of
      Right xs' -> return (uuid, Right (HCons' x' xs'))
      Left  e   -> return (uuid, Left e)
    Left e -> return (uuid, Left e)



-- Translation

-- | Creates the initial network used in the accumulating fold
initialNetwork
  :: forall inputsS inputsT
   . (InitialPipes inputsS inputsT)
  => IO (BasicNetwork inputsS inputsT inputsS inputsT)
initialNetwork = do
  ps <- initialPipes :: IO (PipeList inputsS inputsT)
  return $ BasicNetwork M.empty M.empty ps ps

circuitInputs
  :: ( Length bsS ~ Length bsT
     , ninputs ~ Length bsS
     , IsNat ninputs
     , Network n
     )
  => (N n asS asT) bsS bsT csS csT (ninputs :: Nat)
  -> SNat (Length bsS)
circuitInputs _ = nat


buildBasicNetwork :: InitialPipes a b => Circuit a b c d e -> IO (BasicNetwork a b c d)
buildBasicNetwork x = do
  n  <- icataM5 buildNetworkAlg x
  n' <- initialNetwork
  unN n n'




instance BuildNetworkAlg BasicNetwork Id where
  buildNetworkAlg Id = return (N return)

instance BuildNetworkAlg BasicNetwork Task where
  buildNetworkAlg (Task t) = return $ N
    (\n -> do
      c <- newChan
      let output = PipeCons c PipeNil
      taskUUID <- genUnusedTaskUUID (threads n)
      threadId <- forkIO (taskExecuter (Task t) taskUUID (outputs n) output)
      return $ BasicNetwork (M.insert taskUUID threadId (threads n)) (jobs n) (inputs n) output
    )

instance BuildNetworkAlg BasicNetwork Map where
  buildNetworkAlg (Map (c :: Circuit '[f] '[a] '[g] '[b] N1)) =
    return $ N
      (\n -> do
        outChan <- newChan
        let output = PipeCons outChan PipeNil

        taskUUID <- genUnusedTaskUUID (threads n)
        threadId <- forkIO
          (do
            mapNetwork <-
              startNetwork c :: IO
                ( BasicNetwork
                    '[Var]
                    '[a]
                    '[Var]
                    '[b]
                )
            _ <- forever
              (do
                (jobUUID, mapInputs) <- readPipes (outputs n)
                r                 <-
                  runExceptT
                    (do
                      mapInput             <- (ExceptT . return) mapInputs
                      HCons inputs' HNil <- (lift . fetch') mapInput
                      mapM_ (\x -> do
                                var <- lift emptyVar
                                lift (save var x)
                                lift (write jobUUID (HCons' var HNil') mapNetwork)) inputs'
                      -- input each value into the mapNetwork
                      mapOutput <- mapM
                        (\_ -> do
                          (_, r)             <- lift (read mapNetwork)
                          HCons' out HNil' <- (ExceptT . return) r -- Check for failure
                          lift (fetch out)
                        )
                        inputs'
                      -- get each value out from the mapNetwork
                      outputStore <- lift (empty taskUUID jobUUID)
                      lift (save outputStore mapOutput)
                      return (HCons' outputStore HNil')
                    )

                writePipes jobUUID r output
              )
            stopNetwork mapNetwork
          )


        return $ BasicNetwork (M.insert taskUUID threadId (threads n)) (jobs n) (inputs n) output
      )


instance BuildNetworkAlg BasicNetwork Replicate where
  buildNetworkAlg Replicate = return $ N
    (\n -> do
      output <- dupOutput (outputs n)
      return $ BasicNetwork (threads n) (jobs n) (inputs n) output
    )
   where
    dupOutput :: PipeList '[f] '[a] -> IO (PipeList '[f , f] '[a , a])
    dupOutput (PipeCons c PipeNil) = do
      c' <- dupChan c
      return $ PipeCons c (PipeCons c' PipeNil)

instance BuildNetworkAlg BasicNetwork Then where
  buildNetworkAlg (Then (N x) (N y)) = return $ N
    (x >=> y)

instance BuildNetworkAlg BasicNetwork Swap where
  buildNetworkAlg Swap = return $ N
    (\n -> do
      output <- swapOutput (outputs n)
      return $ BasicNetwork (threads n) (jobs n) (inputs n) output
    )
   where
    swapOutput
      :: PipeList '[f , g] '[a , b] -> IO (PipeList '[g , f] '[b , a])
    swapOutput (PipeCons c1 (PipeCons c2 PipeNil)) = return $ PipeCons c2 (PipeCons c1 PipeNil)

instance BuildNetworkAlg BasicNetwork DropL where
  buildNetworkAlg DropL = return $ N
    (\n -> do
      output <- dropLOutput (outputs n)
      return $ BasicNetwork (threads n) (jobs n) (inputs n) output
    )
   where
    dropLOutput :: PipeList '[f , g] '[a , b] -> IO (PipeList '[g] '[b])
    dropLOutput (PipeCons _ (PipeCons c2 PipeNil)) = return $ PipeCons c2 PipeNil


instance BuildNetworkAlg BasicNetwork DropR where
  buildNetworkAlg DropR = return $ N
    (\n -> do
      output <- dropROutput (outputs n)
      return $ BasicNetwork (threads n) (jobs n) (inputs n) output
    )
   where
    dropROutput :: PipeList '[f , g] '[a , b] -> IO (PipeList '[f] '[a])
    dropROutput (PipeCons c1 (PipeCons _ PipeNil)) = return $ PipeCons c1 PipeNil


instance BuildNetworkAlg BasicNetwork Beside where
  buildNetworkAlg = beside

beside
  :: forall asS asT bsS bsT csS csT (nbs :: Nat)
   . Beside (N BasicNetwork asS asT) bsS bsT csS csT nbs
  -> IO ((N BasicNetwork asS asT) bsS bsT csS csT nbs) -- IO (Network asS asT asA csS csT csA)
beside (Beside l r) = return $ N
  (\n -> do
    let ninputs = circuitInputs l
    (nL  , nR  ) <- splitNetwork ninputs n
    (newL, newR) <- translate ninputs (nL, nR) (l, r)
    joinNetwork (newL, newR)
  )
 where
  splitNetwork
    :: SNat nbsL
    -> BasicNetwork asS asT bsS bsT
    -> IO
         ( BasicNetwork asS asT (Take nbsL bsS) (Take nbsL bsT)
         , BasicNetwork asS asT (Drop nbsL bsS) (Drop nbsL bsT)
         )
  splitNetwork nbs n = return
    ( BasicNetwork (threads n) (jobs n) (inputs n) (takeP nbs (outputs n))
    , BasicNetwork (threads n) (jobs n) (inputs n) (dropP nbs (outputs n))
    )

  translate
    :: SNat nbsL
    -> ( BasicNetwork asS asT (Take nbsL bsS) (Take nbsL bsT)
       , BasicNetwork asS asT (Drop nbsL bsS) (Drop nbsL bsT)
       )
    -> ( (N BasicNetwork asS asT)
           (Take nbsL bsS)
           (Take nbsL bsT)
           csLS
           csLT
           nbsL
       , (N BasicNetwork asS asT)
           (Drop nbsL bsS)
           (Drop nbsL bsT)
           csRS
           csRT
           nbsR
       )
    -> IO
         (BasicNetwork asS asT csLS csLT, BasicNetwork asS asT csRS csRT)
  translate _ (nL, nR) (N cL, N cR) = do
    nL' <- cL nL
    nR' <- cR nR
    return (nL', nR')

  joinNetwork
    :: (AppendP csLS csLT csRS csRT)
    => (BasicNetwork asS asT csLS csLT, BasicNetwork asS asT csRS csRT )
    -> IO (BasicNetwork asS asT (csLS :++ csRS) (csLT :++ csRT))
  joinNetwork (nL, nR) = return
    $ BasicNetwork (threads nL `M.union` threads nR) (jobs nL `M.union` jobs nR) (inputs nL) (outputs nL `appendP` outputs nR)
