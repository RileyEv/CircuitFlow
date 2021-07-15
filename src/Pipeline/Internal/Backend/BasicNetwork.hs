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
import           Data.List                         (nub)
import           Control.Monad.Trans               (lift)
import           Pipeline.Internal.Backend.Network (BuildNetworkAlg (..),
                                                    InitialPipes (..), N (..),
                                                    Network (..))
import           Pipeline.Internal.Common.HList    (HList' (..), HList(..))
import           Pipeline.Internal.Common.IFunctor (icataM7)
import           Pipeline.Internal.Common.Nat      (IsNat (..), Nat,
                                                    SNat (..), N1)
import           Pipeline.Internal.Common.TypeList (Drop, Length, Take,
                                                    (:++))
import           Pipeline.Internal.Core.CircuitAST
import           Pipeline.Internal.Core.Error      (ExceptionMessage (..),
                                                    TaskError (..))
import           Pipeline.Internal.Core.PipeList   (AppendP (..), PipeList (..),
                                                    dropP, takeP)
import           Pipeline.Internal.Core.UUID       (UUID)
import           Pipeline.Internal.Core.DataStore  (Var, emptyVar, DataStore'(..), DataStore(..))
import           Prelude                           hiding (read)

-- | Main type for storing information about the process network.
--
--   The type arguments align with the 'Pipeline.Circuit.Circuit' data type.
data BasicNetwork (inputsStorageType  :: [Type -> Type]) (inputsType  :: [Type]) (inputsAp  :: [Type])
             (outputsStorageType :: [Type -> Type]) (outputsType :: [Type]) (outputsAp :: [Type]) where
  BasicNetwork ::{
    threads :: [ThreadId],
    inputs :: PipeList inputsStorage inputsType inputsAp,
    outputs :: PipeList outputsStorage outputsType outputsAp }
    -> BasicNetwork inputsStorage inputsType inputsAp outputsStorage outputsType outputsAp

instance Network BasicNetwork where
  startNetwork = buildBasicNetwork
  stopNetwork n = forM_ (threads n) killThread
  write uuid xs n = writePipes uuid (Right xs) (inputs n)
  read n = readPipes (outputs n)

-- Task Execution
taskExecuter
  :: Task iF inputsS inputsT inputsA outputS outputT outputsA ninputs
  -> PipeList inputsS inputsT inputsA
  -> PipeList outputS outputT outputA
  -> IO ()
taskExecuter (Task f outStore) inPipes outPipes = forever
  (do
    (uuid, taskInputs) <- readPipes inPipes
    r                  <-
      (runExceptT
        (do
          input <- (ExceptT . return) taskInputs
          catchE (intercept (f uuid input outStore))
                 (throwE . TaskError . ExceptionMessage . displayException)
          return (HCons' outStore HNil')
        )
      )
    writePipes uuid r outPipes
  )


intercept :: ExceptT SomeException IO a -> ExceptT SomeException IO a
intercept a = do
  r <- try a
  case r of
    Right x -> return x
    Left  e -> throwE e


-- Network IO

writePipes
  :: UUID -> Either TaskError (HList' inputsS inputsT) -> PipeList inputsS inputsT inputsA -> IO ()
writePipes _ (Left _) PipeNil = return ()
writePipes uuid (Left e) (PipeCons p ps) =
  writeChan p (uuid, Left e) >> writePipes uuid (Left e) ps
writePipes _ (Right HNil') PipeNil = return ()
writePipes uuid (Right (HCons' x xs)) (PipeCons p ps) =
  writeChan p (uuid, Right x) >> writePipes uuid (Right xs) ps

readPipes
  :: PipeList outputsS outputsT outputsA -> IO (UUID, Either TaskError (HList' outputsS outputsT))
readPipes PipeNil         = return ("", Right HNil')
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
  :: forall inputsS inputsT inputsA
   . (InitialPipes inputsS inputsT inputsA)
  => IO (BasicNetwork inputsS inputsT inputsA inputsS inputsT inputsA)
initialNetwork = do
  ps <- initialPipes :: IO (PipeList inputsS inputsT inputsA)
  return $ BasicNetwork [] ps ps

circuitInputs
  :: ( Length bsS ~ Length bsT
     , Length bsT ~ Length bsA
     , Length bsA ~ Length bsS
     , ninputs ~ Length bsS
     , IsNat ninputs
     , Network n
     )
  => (N n asS asT asA) bsS bsT bsA csS csT csA (ninputs :: Nat)
  -> SNat (Length bsS)
circuitInputs _ = nat


buildBasicNetwork :: InitialPipes a b c => Circuit a b c d e f g -> IO (BasicNetwork a b c d e f)
buildBasicNetwork x = do
  n  <- icataM7 buildNetworkAlg x
  n' <- initialNetwork
  unN n n'




instance BuildNetworkAlg BasicNetwork Id where
  buildNetworkAlg Id = return (N return)

instance BuildNetworkAlg BasicNetwork Task where
  buildNetworkAlg (Task t out) = return $ N
    (\n -> do
      c <- newChan
      let output = PipeCons c PipeNil
      threadId <- forkIO (taskExecuter (Task t out) (outputs n) output)
      return $ BasicNetwork (threadId : threads n) (inputs n) output
    )

instance BuildNetworkAlg BasicNetwork Map where
  buildNetworkAlg (Map (c :: Circuit '[f] '[a] '[f a] '[g] '[b] '[g b] N1) outputStore) =
    return $ N
      (\n -> do
        outChan <- newChan
        let output = PipeCons outChan PipeNil


        threadId <- forkIO
          (do
            mapNetwork <-
              startNetwork c :: IO
                ( BasicNetwork
                    '[Var]
                    '[a]
                    '[Var a]
                    '[Var]
                    '[b]
                    '[Var b]
                )
            _ <- forever
              (do
                (uuid, mapInputs) <- readPipes (outputs n)
                r                 <-
                  (runExceptT
                    (do
                      inputs             <- (ExceptT . return) mapInputs
                      HCons inputs' HNil <- (lift . fetch' uuid) inputs

                      mapM_ (\x -> do
                                var <- lift emptyVar
                                lift (save uuid var x)
                                lift (write uuid (HCons' var HNil') mapNetwork)) inputs'
                      -- input each value into the mapNetwork
                      output <- mapM
                        (\x -> do
                          (uuid, r)             <- lift (read mapNetwork)
                          HCons' out HNil' <- (ExceptT . return) r -- Check for failure
                          r' <- lift (fetch uuid out)
                          return r'
                        )
                        inputs'
                      -- get each value out from the mapNetwork
                      lift (save uuid outputStore output)
                      return (HCons' outputStore HNil')
                    )
                  )
                writePipes uuid r output
              )
            stopNetwork mapNetwork
          )

        -- threadId <- forkIO (taskExecuter (Task t out) (outputs n) output)

        return $ BasicNetwork (threadId : threads n) (inputs n) output
      )



instance BuildNetworkAlg BasicNetwork Replicate where
  buildNetworkAlg Replicate = return $ N
    (\n -> do
      output <- dupOutput (outputs n)
      return $ BasicNetwork (threads n) (inputs n) output
    )
   where
    dupOutput :: PipeList '[f] '[a] '[f a] -> IO (PipeList '[f , f] '[a , a] '[f a , f a])
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
      return $ BasicNetwork (threads n) (inputs n) output
    )
   where
    swapOutput
      :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[g , f] '[b , a] '[g b , f a])
    swapOutput (PipeCons c1 (PipeCons c2 PipeNil)) = return $ PipeCons c2 (PipeCons c1 PipeNil)

instance BuildNetworkAlg BasicNetwork DropL where
  buildNetworkAlg DropL = return $ N
    (\n -> do
      output <- dropLOutput (outputs n)
      return $ BasicNetwork (threads n) (inputs n) output
    )
   where
    dropLOutput :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[g] '[b] '[g b])
    dropLOutput (PipeCons _ (PipeCons c2 PipeNil)) = return $ PipeCons c2 PipeNil


instance BuildNetworkAlg BasicNetwork DropR where
  buildNetworkAlg DropR = return $ N
    (\n -> do
      output <- dropROutput (outputs n)
      return $ BasicNetwork (threads n) (inputs n) output
    )
   where
    dropROutput :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[f] '[a] '[f a])
    dropROutput (PipeCons c1 (PipeCons _ PipeNil)) = return $ PipeCons c1 PipeNil


instance BuildNetworkAlg BasicNetwork Beside where
  buildNetworkAlg = beside

beside
  :: forall asS asT asA bsS bsT bsA csS csT csA (nbs :: Nat)
   . Beside (N BasicNetwork asS asT asA) bsS bsT bsA csS csT csA nbs
  -> IO ((N BasicNetwork asS asT asA) bsS bsT bsA csS csT csA nbs) -- IO (Network asS asT asA csS csT csA)
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
    -> BasicNetwork asS asT asA bsS bsT bsA
    -> IO
         ( BasicNetwork asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA)
         , BasicNetwork asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA)
         )
  splitNetwork nbs n = return
    ( BasicNetwork (threads n) (inputs n) (takeP nbs (outputs n))
    , BasicNetwork (threads n) (inputs n) (dropP nbs (outputs n))
    )

  translate
    :: SNat nbsL
    -> ( BasicNetwork asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA)
       , BasicNetwork asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA)
       )
    -> ( (N BasicNetwork asS asT asA)
           (Take nbsL bsS)
           (Take nbsL bsT)
           (Take nbsL bsA)
           csLS
           csLT
           csLA
           nbsL
       , (N BasicNetwork asS asT asA)
           (Drop nbsL bsS)
           (Drop nbsL bsT)
           (Drop nbsL bsA)
           csRS
           csRT
           csRA
           nbsR
       )
    -> IO
         (BasicNetwork asS asT asA csLS csLT csLA, BasicNetwork asS asT asA csRS csRT csRA)
  translate _ (nL, nR) (N cL, N cR) = do
    nL' <- cL nL
    nR' <- cR nR
    return (nL', nR')

  joinNetwork
    :: (AppendP csLS csLT csLA csRS csRT csRA)
    => (BasicNetwork asS asT asA csLS csLT csLA, BasicNetwork asS asT asA csRS csRT csRA)
    -> IO (BasicNetwork asS asT asA (csLS :++ csRS) (csLT :++ csRT) (csLA :++ csRA))
  joinNetwork (nL, nR) = return
    $ BasicNetwork (nub (threads nL ++ threads nR)) (inputs nL) (outputs nL `appendP` outputs nR)
