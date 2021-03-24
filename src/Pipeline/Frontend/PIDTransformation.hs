
{-# LANGUAGE PolyKinds
           , TypeFamilies
           , MultiParamTypeClasses
#-}
module Pipeline.Frontend.PIDTransformation where

import Pipeline.Core.Modular ((:+:)(..), (:<:)(..))
import Pipeline.Core.IFunctor (IFix2(..), IFunctor2(..))
import Pipeline.Core.Task (TaskF(..))
import Pipeline.Core.PID (PID, PIDF(..))

import Pipeline.Frontend.Circuit (Circuit', Replicate(..), Id(..), Beside(..), Then(..), Swap(..), DropL(..), DropR(..))

import Control.Monad.State (State, get, put, runState)


-- Transforming the circuit of tasks into a circuit containing PIDs instead.

-- |A monad that is used when defining a pipeline.
type PIDState = State PID

nextPID :: PIDState PID
nextPID = do
  s <- get
  let newPID = s + 1
  put newPID
  return newPID

circuitPIDTransform :: Circuit' TaskF inputs outputs -> (Circuit' PIDF inputs outputs, PID)
circuitPIDTransform (IIn2 c) = runState (injectPIDs c) (-1) 
 

class IFunctor2 iF => InjectPIDs iF where
  injectPIDs :: iF (Circuit' TaskF) inputs outputs
       -> PIDState (Circuit' PIDF inputs outputs)


instance (InjectPIDs iF, InjectPIDs iG) => InjectPIDs (iF :+: iG) where
  injectPIDs (L x) = injectPIDs x
  injectPIDs (R y) = injectPIDs y

instance InjectPIDs Id where
  injectPIDs Id = return ((IIn2 . inj) Id)

instance InjectPIDs TaskF where
  injectPIDs (TaskF _ _) = IIn2 . inj . PIDF <$> nextPID

instance InjectPIDs Replicate where
  injectPIDs Replicate = return ((IIn2 . inj) Replicate)

instance InjectPIDs Beside where
  injectPIDs (Beside (IIn2 l) (IIn2 r)) = do
    l' <- injectPIDs l
    r' <- injectPIDs r
    return $ (IIn2 . inj) (Beside l' r')

instance InjectPIDs Then where
  injectPIDs (Then (IIn2 x) (IIn2 y)) = do
    x' <- injectPIDs x
    y' <- injectPIDs y
    return $ (IIn2 . inj) (Then x' y')

instance InjectPIDs Swap where
  injectPIDs Swap = return $ (IIn2 . inj) Swap

instance InjectPIDs DropL where
  injectPIDs DropL = return $ (IIn2 . inj) DropL

instance InjectPIDs DropR where
  injectPIDs DropR = return $ (IIn2 . inj) DropR


