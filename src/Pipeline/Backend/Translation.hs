{-# LANGUAGE ScopedTypeVariables #-}
module Pipeline.Backend.Translation where

import Pipeline.Core.Task (TaskF(..))
import Pipeline.Core.IFunctor (IFunctor2, IFix2(..))
import Pipeline.Core.Modular ((:+:)(..))
import Pipeline.Core.DataStore (HAppendListR)
import Pipeline.Core.Nat (SNat(..), Take, Drop, Length)

import Pipeline.Frontend.Circuit

import Pipeline.Backend.ProcessNetwork (Network(..), PipeList(..), taskExecuter)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, dupChan)

import Unsafe.Coerce (unsafeCoerce)

-- Used to build a list of pipes from a list of types.
class InitialPipes (inputs :: [*]) where
  initialPipes :: IO (PipeList inputs)

instance InitialPipes is => InitialPipes (i ': is) where
  initialPipes = do
    c <- newChan
    PipeCons c <$> (initialPipes :: IO (PipeList is))

instance InitialPipes '[] where
  initialPipes = return PipeNil

-- | Creates the initial network used in the accumulating fold
initialNetwork :: forall inputs. (InitialPipes inputs) => IO (Network inputs inputs)
initialNetwork = do
  ps <- (initialPipes :: IO (PipeList inputs))
  return $ Network [] ps ps 


-- | The accumulating fold to build the network.
class IFunctor2 iF => BuildNetwork iF where
  buildNetwork :: Network as bs
    -> iF (Circuit' TaskF) bs cs
    -> IO (Network as cs)

instance (BuildNetwork iF, BuildNetwork iG) => BuildNetwork (iF :+: iG) where
  buildNetwork n (L x) = buildNetwork n x
  buildNetwork n (R y) = buildNetwork n y

instance BuildNetwork Id where
  buildNetwork n Id = return n

instance BuildNetwork TaskF where
  buildNetwork n (TaskF t out) = do
    c <- newChan
    let output = PipeCons c PipeNil
    threadId <- forkIO (taskExecuter (IIn2 $ TaskF t out) (outputs n) output)
    return $ Network (threadId : threads n) (inputs n) output

instance BuildNetwork Then where
  buildNetwork n (Then (IIn2 x) (IIn2 y)) = do
    nx <- buildNetwork n x
    buildNetwork nx y

instance BuildNetwork Replicate where
  buildNetwork n Replicate = do
    output <- dupOutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      dupOutput :: PipeList '[a] -> IO (PipeList '[a, a])
      dupOutput (PipeCons c PipeNil) = do
        c' <- dupChan c
        return $ PipeCons c (PipeCons c' PipeNil)

instance BuildNetwork Swap where
  buildNetwork n Swap = do
    output <- swapOutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      swapOutput :: PipeList '[a, b] -> IO (PipeList '[b, a])
      swapOutput (PipeCons c1 (PipeCons c2 PipeNil)) = return $ PipeCons c2 (PipeCons c1 PipeNil)

  
instance BuildNetwork DropL where
  buildNetwork n DropL = do
    output <- dropLOutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      dropLOutput :: PipeList '[a, b] -> IO (PipeList '[b])
      dropLOutput (PipeCons _ (PipeCons c2 PipeNil)) = return $ PipeCons c2 PipeNil

instance BuildNetwork DropR where
  buildNetwork n DropR = do
    output <- dropROutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      dropROutput :: PipeList '[a, b] -> IO (PipeList '[a])
      dropROutput (PipeCons c1 (PipeCons _ PipeNil)) = return $ PipeCons c1 PipeNil

instance BuildNetwork Beside where
  buildNetwork n (Beside (IIn2 l) (IIn2 r)) = do
    let nInL = circuitInputs (IIn2 l)
        nOutL = circuitOutputs (IIn2 r)
    (n1, n2) <- splitNetwork n nInL nOutL
    n1' <- buildNetwork n1 l
    n2' <- buildNetwork n2 r
    joinNetwork n1' n2'
    where
      splitNetwork :: (HAppendListR (Take ninputs inputs)   (Drop ninputs inputs) ~ inputs,
                       HAppendListR (Take noutputs outputs) (Drop noutputs outputs) ~ outputs)
                   => Network inputs outputs
                   -> SNat ninputs
                   -> SNat noutputs
                   -> IO (Network (Take ninputs inputs) (Take noutputs outputs), Network (Drop ninputs inputs) (Drop noutputs outputs))
      splitNetwork = undefined
      joinNetwork :: Network in1 out1 -> Network in2 out2 -> IO (Network (HAppendListR in1 in2) (HAppendListR out1 out2))
      joinNetwork = undefined

circuitInputs :: (ninputs ~ Length inputs) => Circuit inputs outputs -> SNat ninputs
circuitInputs = undefined
circuitOutputs :: (noutputs ~ Length outputs) => Circuit input outputs -> SNat noutputs
circuitOutputs = undefined
