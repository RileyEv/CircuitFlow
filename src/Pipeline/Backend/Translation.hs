{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}
module Pipeline.Backend.Translation where

import Pipeline.Core.Task (TaskF(..))
import Pipeline.Core.IFunctor (IFunctor4, IFix4(..))
import Pipeline.Core.Modular ((:+:)(..))
import Pipeline.Core.HList
import Pipeline.Core.DataStore (type (++), Apply, DataSource')
import Pipeline.Core.Nat (SNat(..), Take, Drop, Length, (:<=), (==>))

import Pipeline.Frontend.Circuit

import Pipeline.Backend.ProcessNetwork (Network(..), PipeList(..), taskExecuter, takeP , dropP, appendP)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, dupChan, Chan)

import Data.Type.Equality (gcastWith, (:~:)(..))
import Data.List (nub)

import Unsafe.Coerce (unsafeCoerce)

-- Used to build a list of pipes from a list of types.
class InitialPipes (inputsS :: [* -> *]) (inputsT :: [*]) where
  initialPipes :: IO (PipeList inputsS inputsT)

instance InitialPipes fs as => InitialPipes (f ': fs) (a ': as) where
  initialPipes = do
    c <- (newChan :: IO (Chan (f a)))
    PipeCons c <$> (initialPipes :: IO (PipeList fs as))

instance InitialPipes '[] '[] where
  initialPipes = return PipeNil

-- | Creates the initial network used in the accumulating fold
initialNetwork :: forall inputsS inputsT. (InitialPipes inputsS inputsT) => IO (Network inputsS inputsT inputsS inputsT)
initialNetwork = do
  ps <- (initialPipes :: IO (PipeList inputsS inputsT))
  return $ Network [] ps ps 


-- | The accumulating fold to build the network.
class IFunctor4 iF => BuildNetwork iF where
  buildNetwork :: Network asS asT bsS bsT
    -> iF (Circuit' TaskF) bsS bsT csS csT
    -> IO (Network asS asT csS csT)

instance (BuildNetwork iF, BuildNetwork iG) => BuildNetwork (iF :+: iG) where
  buildNetwork n (L x) = buildNetwork n x
  buildNetwork n (R y) = buildNetwork n y

instance BuildNetwork Id where
  buildNetwork n Id = return n

instance BuildNetwork TaskF where
  buildNetwork n (TaskF t out) = do
    c <- newChan
    let output = PipeCons c PipeNil
    threadId <- forkIO (taskExecuter (IIn4 $ TaskF t out) (outputs n) output)
    return $ Network (threadId : threads n) (inputs n) output

instance BuildNetwork Then where
  buildNetwork n (Then (IIn4 x) (IIn4 y)) = do
    nx <- buildNetwork n x
    buildNetwork nx y

instance BuildNetwork Replicate where
  buildNetwork n Replicate = do
    output <- dupOutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      dupOutput :: PipeList '[f] '[a] -> IO (PipeList '[f, f] '[a, a])
      dupOutput (PipeCons c PipeNil) = do
        c' <- dupChan c
        return $ PipeCons c (PipeCons c' PipeNil)

instance BuildNetwork Swap where
  buildNetwork n Swap = do
    output <- swapOutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      swapOutput :: PipeList '[f, g] '[a, b] -> IO (PipeList '[g, f] '[b, a])
      swapOutput (PipeCons c1 (PipeCons c2 PipeNil)) = return $ PipeCons c2 (PipeCons c1 PipeNil)
  
instance BuildNetwork DropL where
  buildNetwork n DropL = do
    output <- dropLOutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      dropLOutput :: PipeList '[f, g] '[a, b] -> IO (PipeList '[g] '[b])
      dropLOutput (PipeCons _ (PipeCons c2 PipeNil)) = return $ PipeCons c2 PipeNil

instance BuildNetwork DropR where
  buildNetwork n DropR = do
    output <- dropROutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      dropROutput :: PipeList '[f, g] '[a, b] -> IO (PipeList '[f] '[a])
      dropROutput (PipeCons c1 (PipeCons _ PipeNil)) = return $ PipeCons c1 PipeNil

instance BuildNetwork Beside where
  buildNetwork = beside

beside :: forall asS asT bsS bsT csS csT.
  Network asS asT bsS bsT -> Beside Circuit bsS bsT csS csT -> IO (Network asS asT csS csT)
beside n (Beside l r) = do
  let ninputsS = circuitInputsS l
      ninputsT = circuitInputsT l
  (nL, nR) <- splitNetwork ninputsS ninputsT
  (newL, newR) <- translate ninputsS ninputsT (nL, nR) (
    gcastWith (takeLengthAppendProofT ninputsT l r) (gcastWith (takeLengthAppendProofS ninputsS l r) l),
    gcastWith (dropLengthAppendProofT ninputsT l r) (gcastWith (dropLengthAppendProofS ninputsS l r) r))
  gcastWith (applyAppendProof (outputs newL) (outputs newR)) $ joinNetwork (newL, newR)
  where
    splitNetwork :: SNat nbsLS -> SNat nbsLT -> IO (Network asS asT (Take nbsLS bsS) (Take nbsLT bsT), Network asS asT (Drop nbsLS bsS) (Drop nbsLT bsT))
    splitNetwork nbsS nbsT = return (Network (threads n) (inputs n) (takeP nbsS nbsT (outputs n)), Network (threads n) (inputs n) (dropP nbsS nbsT (outputs n)))

    translate :: SNat nbsLS -> SNat nbsLT
      -> (Network asS asT (Take nbsLS bsS) (Take nbsLT bsT), Network asS asT (Drop nbsLS bsS) (Drop nbsLT bsT))
      -> (Circuit (Take nbsLS bsS) (Take nbsLT bsT) csLS csLT, Circuit (Drop nbsLS bsS) (Drop nbsLT bsT) csRS csRT)
      -> IO (Network asS asT csLS csLT, Network asS asT csRS csRT)
    translate _ _ (nL, nR) (IIn4 cL, IIn4 cR) = do
      nL' <- buildNetwork nL cL
      nR' <- buildNetwork nR cR
      return (nL', nR')

    joinNetwork :: (Network asS asT csLS csLT, Network asS asT csRS csRT) -> IO (Network asS asT (csLS ++ csRS) (csLT ++ csRT))
    joinNetwork (nL, nR) = return $ Network (nub (threads nL ++ threads nR)) (inputs nL) (outputs nL `appendP` outputs nR)


-- data Network in out = Network [ThreadIds] (PipeList in) (PipeList out)

-- Network as bs  = Network as  bsL ` join ` Network as  bsR
-- Circuit bs cs  = Circuit bsL csL ` join ` Circuit bsR csR

-- Network as cs  = Network as  csL ` join ` Network as  csR

circuitInputsS :: Circuit inputsS inputsT outputsS outputsT -> SNat (Length inputsS)
circuitInputsS = undefined
circuitInputsT :: Circuit inputsS inputsT outputsS outputsT -> SNat (Length inputsT)
circuitInputsT = undefined


-- applyAppendLId :: PipeList (Apply fs as) -> Apply fs as ++ '[] :~: Apply fs as
-- applyAppendLId PipeNil = Refl
-- applyAppendLId (PipeCons x xs) = gcastWith (applyAppendLId xs) Refl

takeLengthAppendProofS :: SNat (Length fs) -> Circuit fs as gs bs -> Circuit hs cs ds is -> Take (Length fs) (fs ++ hs) :~: fs
takeLengthAppendProofS SZero = undefined

takeLengthAppendProofT :: SNat (Length as) -> Circuit fs as gs bs -> Circuit hs cs is ds -> Take (Length as) (as ++ cs) :~: as
takeLengthAppendProofT SZero _ = undefined

dropLengthAppendProofS :: SNat (Length fs) -> Circuit fs as gs bs -> Circuit hs cs ds is -> Drop (Length fs) (fs ++ hs) :~: hs
dropLengthAppendProofS SZero = undefined

dropLengthAppendProofT :: SNat (Length as) -> Circuit fs as gs bs -> Circuit hs cs is ds -> Drop (Length as) (as ++ cs) :~: cs
dropLengthAppendProofT SZero _ = undefined

-- dropLengthAppendProof (SSucc n) (PipeCons x xs) ys = gcastWith (dropLengthAppendProof n xs ys) Refl

applyAppendProof :: PipeList fs as -> PipeList gs bs -> Apply fs as ++ Apply gs bs :~: Apply (fs ++ gs) (as ++ bs)
applyAppendProof PipeNil _ = Refl
applyAppendProof (PipeCons x xs) ys = proof x xs ys
  where
    proof :: forall f a fs as gs bs. Chan (f a)
      -> PipeList fs as
      -> PipeList gs bs
      -> Apply (f ': fs) (a ': as) ++ Apply gs bs :~: Apply ((f ': fs) ++ gs) ((a ': as) ++ bs)
    proof x xs ys = p1 ==> p2 ==> p3 ==> p4 ==> p5
      where
        p1 :: Apply (f ': fs) (a ': as) ++ Apply gs bs :~: (f a ': Apply fs as) ++ Apply gs bs
        p1 = Refl
        
        p2 :: (f a ': Apply fs as) ++ Apply gs bs :~: f a ': (Apply fs as ++ Apply gs bs)
        p2 = gcastWith (consAppendAssoc x xs ys) Refl
        
        p3 :: f a ': (Apply fs as ++ Apply gs bs) :~: f a ': (Apply (fs ++ gs) (as ++ bs))
        p3 = gcastWith (applyAppendProof xs ys) Refl
        
        p4 :: f a ': (Apply (fs ++ gs) (as ++ bs)) :~: Apply (f ': (fs ++ gs)) (a ': (as ++ bs))
        p4 = Refl
        
        p5 :: Apply (f ': (fs ++ gs)) (a ': (as ++ bs)) :~: Apply ((f ': fs) ++ gs) ((a ': as) ++ bs)
        p5 = gcastWith (applyConsAppendAssoc x xs ys) Refl

    consAppendAssoc :: Chan (f a)
      -> PipeList fs as
      -> PipeList gs bs
      -> (f a ': Apply fs as) ++ Apply gs bs :~: f a ': (Apply fs as ++ Apply gs bs)
    consAppendAssoc c PipeNil ys = Refl
    consAppendAssoc c (PipeCons x xs) ys = unsafeCoerce Refl -- TODO: Eliminate this

    applyConsAppendAssoc :: Chan (f a)
      -> PipeList fs as
      -> PipeList gs bs
      -> Apply (f ': (fs ++ gs)) (a ': (as ++ bs)) :~: Apply ((f ': fs) ++ gs) ((a ': as) ++ bs)
    applyConsAppendAssoc c PipeNil ys = Refl
    applyConsAppendAssoc c (PipeCons x xs) ys = unsafeCoerce Refl -- TODO: Eliminate this
      

-- consAppendAssoc :: PipeList (f ': fs) (a ': as) -> PipeList gs bs -> Apply (f ': fs) (a ': as) ++ Apply gs bs :~: f a ': (Apply fs as ++ Apply gs bs)
-- consAppendAssoc (PipeCons x PipeNil) ys = undefined
-- consAppendAssoc (PipeCons x xs@(PipeCons _ _)) ys = gcastWith (consAppendAssoc xs ys) Refl
-- consAppendAssoc (PipeCons x xs@(PipeCons _ _)) ys = gcastWith (consAppendAssoc xs ys) Refl

-- PROOOOOOOF: https://stackoverflow.com/questions/59455253/how-do-i-prove-type-level-list-properties-in-haskell
-- nah it doesnt work
