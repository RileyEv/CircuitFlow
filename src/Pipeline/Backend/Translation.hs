{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}
module Pipeline.Backend.Translation where

import Pipeline.Core.Task (TaskF(..))
import Pipeline.Core.IFunctor (IFunctor7, IFix7(..))
import Pipeline.Core.Modular ((:+:)(..))
import Pipeline.Core.HList
import Pipeline.Core.DataStore ((:++), Apply, DataSource')
import Pipeline.Core.Nat (SNat(..), Take, Drop, Length, (:<=), (==>), (!+), nat, IsNat)

import Pipeline.Frontend.Circuit

import Pipeline.Backend.ProcessNetwork (Network(..), PipeList(..), taskExecuter, takeP , dropP, appendP, AppendP)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, dupChan, Chan)

import Data.Type.Equality (gcastWith, (:~:)(..))
import Data.List (nub)

import Unsafe.Coerce (unsafeCoerce)

-- Used to build a list of pipes from a list of types.
class InitialPipes (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*]) where
  initialPipes :: IO (PipeList inputsS inputsT inputsA)

instance InitialPipes fs as xs => InitialPipes (f ': fs) (a ': as) (f a ': xs) where
  initialPipes = do
    c <- (newChan :: IO (Chan (f a)))
    PipeCons c <$> (initialPipes :: IO (PipeList fs as xs))

instance InitialPipes '[] '[] '[] where
  initialPipes = return PipeNil

-- | Creates the initial network used in the accumulating fold
initialNetwork :: forall inputsS inputsT inputsA. (InitialPipes inputsS inputsT inputsA) => IO (Network inputsS inputsT inputsA inputsS inputsT inputsA)
initialNetwork = do
  ps <- (initialPipes :: IO (PipeList inputsS inputsT inputsA))
  return $ Network [] ps ps 


-- | The accumulating fold to build the network.
class IFunctor7 iF => BuildNetwork iF where
  buildNetwork :: Network asS asT asA bsS bsT bsA
    -> iF (Circuit' TaskF) bsS bsT bsA csS csT csA nbs
    -> IO (Network asS asT asA csS csT csA)

instance (BuildNetwork iF, BuildNetwork iG) => BuildNetwork (iF :+: iG) where
  buildNetwork n (L x) = buildNetwork n x
  buildNetwork n (R y) = buildNetwork n y

instance BuildNetwork Id where
  buildNetwork n Id = return n

instance BuildNetwork TaskF where
  buildNetwork n (TaskF t out) = do
    c <- newChan
    let output = PipeCons c PipeNil
    threadId <- forkIO (taskExecuter (TaskF t out) (outputs n) output)
    return $ Network (threadId : threads n) (inputs n) output

instance BuildNetwork Then where
  buildNetwork n (Then (IIn7 x) (IIn7 y)) = do
    nx <- buildNetwork n x
    buildNetwork nx y

instance BuildNetwork Replicate where
  buildNetwork n Replicate = do
    output <- dupOutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      dupOutput :: PipeList '[f] '[a] '[f a] -> IO (PipeList '[f, f] '[a, a] '[f a, f a])
      dupOutput (PipeCons c PipeNil) = do
        c' <- dupChan c
        return $ PipeCons c (PipeCons c' PipeNil)

instance BuildNetwork Swap where
  buildNetwork n Swap = do
    output <- swapOutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      swapOutput :: PipeList '[f, g] '[a, b] '[f a, g b] -> IO (PipeList '[g, f] '[b, a] '[g b, f a])
      swapOutput (PipeCons c1 (PipeCons c2 PipeNil)) = return $ PipeCons c2 (PipeCons c1 PipeNil)
  
instance BuildNetwork DropL where
  buildNetwork n DropL = do
    output <- dropLOutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      dropLOutput :: PipeList '[f, g] '[a, b] '[f a, g b] -> IO (PipeList '[g] '[b] '[g b])
      dropLOutput (PipeCons _ (PipeCons c2 PipeNil)) = return $ PipeCons c2 PipeNil

instance BuildNetwork DropR where
  buildNetwork n DropR = do
    output <- dropROutput (outputs n)
    return $ Network (threads n) (inputs n) output
    where
      dropROutput :: PipeList '[f, g] '[a, b] '[f a, g b] -> IO (PipeList '[f] '[a] '[f a])
      dropROutput (PipeCons c1 (PipeCons _ PipeNil)) = return $ PipeCons c1 PipeNil

instance BuildNetwork Beside where
  buildNetwork = beside

beside :: forall asS asT asA bsS bsT bsA csS csT csA nbs.
  Network asS asT asA bsS bsT bsA -> Beside Circuit bsS bsT bsA csS csT csA nbs -> IO (Network asS asT asA csS csT csA)
beside n (Beside l@(IIn7 l') r) = do
  let ninputs = circuitInputs l
  (nL, nR) <- splitNetwork ninputs
  (newL, newR) <- translate ninputs (nL, nR) (l, r)
  joinNetwork (newL, newR)
  where
    splitNetwork :: nbsLS ~ nbsLT
      => SNat nbsL
      -> IO (Network asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA), Network asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA))
    splitNetwork nbs = return (Network (threads n) (inputs n) (takeP nbs (outputs n)), Network (threads n) (inputs n) (dropP nbs (outputs n)))

    translate :: SNat nbsL
      -> (Network asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA), Network asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA))
      -> (Circuit (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA) csLS csLT csLA nbsL,
          Circuit (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA) csRS csRT csRA nbsR)
      -> IO (Network asS asT asA csLS csLT csLA, Network asS asT asA csRS csRT csRA)
    translate _ (nL, nR) (IIn7 cL, IIn7 cR) = do
      nL' <- buildNetwork nL cL
      nR' <- buildNetwork nR cR
      return (nL', nR')

    joinNetwork :: (AppendP csLS csLT csLA csRS csRT csRA) => (Network asS asT asA csLS csLT csLA, Network asS asT asA csRS csRT csRA)
      -> IO (Network asS asT asA (csLS :++ csRS) (csLT :++ csRT) (csLA :++ csRA))
    joinNetwork (nL, nR) = return $ Network (nub (threads nL ++ threads nR)) (inputs nL) (outputs nL `appendP` outputs nR)


-- data Network in out = Network [ThreadIds] (PipeList in) (PipeList out)

-- Network as bs  = Network as  bsL ` join ` Network as  bsR
-- Circuit bs cs  = Circuit bsL csL ` join ` Circuit bsR csR

-- Network as cs  = Network as  csL ` join ` Network as  csR


circuitInputs :: (Length inputsS ~ Length inputsT,
                  Length inputsT ~ Length inputsA,
                  Length inputsA ~ Length inputsS,
                  ninputs ~ Length inputsS, IsNat ninputs) => Circuit inputsS inputsT inputsA outputsS outputsT outputsA ninputs -> (SNat (Length inputsS))
circuitInputs c = nat


-- class IFunctor7 iF => CircuitInputs iF where
--   circuitInputs :: (Length inputsS ~ Length inputsT,
--                     Length inputsT ~ Length inputsA,
--                     Length inputsA ~ Length inputsS,
--                     Length inputsS ~ ninputs)
--     => iF Circuit inputsS inputsT inputsA outputsS outputsT outputsA ninputs -> SNat (Length inputsS)

-- instance  (CircuitInputs iF, CircuitInputs iG) => CircuitInputs (iF :+: iG) where
--   circuitInputs (L x) = circuitInputs x
--   circuitInputs (R y) = circuitInputs y

-- instance CircuitInputs Id where
--   circuitInputs _ = SSucc SZero

-- instance CircuitInputs TaskF where
--   circuitInputs (TaskF _ _) = undefined

-- instance CircuitInputs Then where
--   circuitInputs (Then (IIn7 t) _) = circuitInputs t

-- instance CircuitInputs Beside where
--   circuitInputs (Beside (IIn7 l) (IIn7 r)) = circuitInputs l !+ circuitInputs r

-- instance CircuitInputs Replicate where
--   circuitInputs _ = SSucc SZero

-- instance CircuitInputs Swap where
--   circuitInputs _ = SSucc (SSucc SZero)

-- instance CircuitInputs DropL where
--   circuitInputs _ = SSucc (SSucc SZero)
  
-- instance CircuitInputs DropR where
--   circuitInputs _ = SSucc (SSucc SZero)

-- applyAppendLId :: PipeList (Apply fs as) -> Apply fs as ++ '[] :~: Apply fs as
-- applyAppendLId PipeNil = Refl
-- applyAppendLId (PipeCons x xs) = gcastWith (applyAppendLId xs) Refl

-- takeLengthAppendProofS :: SNat (Length fs) -> Circuit fs as gs bs -> Circuit hs cs ds is -> Take (Length fs) (fs :++ hs) :~: fs
-- takeLengthAppendProofS SZero = undefined

-- takeLengthAppendProofT :: SNat (Length as) -> Circuit fs as gs bs -> Circuit hs cs is ds -> Take (Length as) (as :++ cs) :~: as
-- takeLengthAppendProofT SZero _ = undefined

-- dropLengthAppendProofS :: SNat (Length fs) -> Circuit fs as gs bs -> Circuit hs cs ds is -> Drop (Length fs) (fs :++ hs) :~: hs
-- dropLengthAppendProofS SZero = undefined

-- dropLengthAppendProofT :: SNat (Length as) -> Circuit fs as gs bs -> Circuit hs cs is ds -> Drop (Length as) (as :++ cs) :~: cs
-- dropLengthAppendProofT SZero _ = undefined

-- dropLengthAppendProof (SSucc n) (PipeCons x xs) ys = gcastWith (dropLengthAppendProof n xs ys) Refl

-- applyAppendProof :: PipeList fs as (Apply fs as) -> PipeList gs bs (Apply gs bs) -> Apply fs as :++ Apply gs bs :~: Apply (fs :++ gs) (as :++ bs)
-- applyAppendProof PipeNil _ = Refl
-- applyAppendProof (PipeCons x xs) ys = proof x xs ys
--   where
--     proof :: forall f a fs as gs bs. Chan (f a)
--       -> PipeList fs as (Apply fs as)
--       -> PipeList gs bs (Apply gs bs)
--       -> Apply (f ': fs) (a ': as) :++ Apply gs bs :~: Apply ((f ': fs) :++ gs) ((a ': as) :++ bs)
--     proof x xs ys = p1 ==> p2 ==> p3 ==> p4 ==> p5
--       where
--         p1 :: Apply (f ': fs) (a ': as) :++ Apply gs bs :~: (f a ': Apply fs as) :++ Apply gs bs
--         p1 = Refl
        
--         p2 :: (f a ': Apply fs as) :++ Apply gs bs :~: f a ': (Apply fs as :++ Apply gs bs)
--         p2 = gcastWith (consAppendAssoc x xs ys) Refl
        
--         p3 :: f a ': (Apply fs as :++ Apply gs bs) :~: f a ': (Apply (fs :++ gs) (as :++ bs))
--         p3 = gcastWith (applyAppendProof xs ys) Refl
        
--         p4 :: f a ': (Apply (fs :++ gs) (as :++ bs)) :~: Apply (f ': (fs :++ gs)) (a ': (as :++ bs))
--         p4 = Refl
        
--         p5 :: Apply (f ': (fs :++ gs)) (a ': (as :++ bs)) :~: Apply ((f ': fs) :++ gs) ((a ': as) :++ bs)
--         p5 = gcastWith (applyConsAppendAssoc x xs ys) Refl

--     consAppendAssoc :: Chan (f a)
--       -> PipeList fs as (Apply fs as)
--       -> PipeList gs bs (Apply gs bs)
--       -> (f a ': Apply fs as) :++ Apply gs bs :~: f a ': (Apply fs as :++ Apply gs bs)
--     consAppendAssoc c PipeNil ys = Refl
--     consAppendAssoc c (PipeCons x xs) ys = unsafeCoerce Refl -- TODO: Eliminate this

--     applyConsAppendAssoc :: Chan (f a)
--       -> PipeList fs as (Apply fs as)
--       -> PipeList gs bs (Apply gs bs)
--       -> Apply (f ': (fs :++ gs)) (a ': (as :++ bs)) :~: Apply ((f ': fs) :++ gs) ((a ': as) :++ bs)
--     applyConsAppendAssoc c PipeNil ys = Refl
--     applyConsAppendAssoc c (PipeCons x xs) ys = unsafeCoerce Refl -- TODO: Eliminate this
      

-- consAppendAssoc :: PipeList (f ': fs) (a ': as) -> PipeList gs bs -> Apply (f ': fs) (a ': as) ++ Apply gs bs :~: f a ': (Apply fs as ++ Apply gs bs)
-- consAppendAssoc (PipeCons x PipeNil) ys = undefined
-- consAppendAssoc (PipeCons x xs@(PipeCons _ _)) ys = gcastWith (consAppendAssoc xs ys) Refl
-- consAppendAssoc (PipeCons x xs@(PipeCons _ _)) ys = gcastWith (consAppendAssoc xs ys) Refl

-- PROOOOOOOF: https://stackoverflow.com/questions/59455253/how-do-i-prove-type-level-list-properties-in-haskell
-- nah it doesnt work
