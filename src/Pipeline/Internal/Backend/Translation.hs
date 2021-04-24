{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pipeline.Internal.Backend.Translation where

import           Control.Concurrent                        (forkIO)
import           Control.Concurrent.Chan                   (Chan, dupChan,
                                                            newChan)
import           Data.Kind                                 (Type)
import           Data.List                                 (nub)
import           Pipeline.Internal.Backend.ProcessNetwork  (Network (..),
                                                            taskExecuter)
import           Pipeline.Internal.Common.IFunctor         (IFix7 (..),
                                                            IFunctor7, icataM7)
import           Pipeline.Internal.Common.IFunctor.Modular ((:+:) (..))
import           Pipeline.Internal.Common.Nat              (IsNat (..), Nat,
                                                            SNat (..))
import           Pipeline.Internal.Common.TypeList         (Drop, Length, Take,
                                                            (:++))
import           Pipeline.Internal.Core.CircuitAST
import           Pipeline.Internal.Core.Error              (TaskError)
import           Pipeline.Internal.Core.PipeList           (AppendP (..),
                                                            PipeList (..),
                                                            dropP, takeP)
import           Pipeline.Internal.Core.UUID               (UUID)



-- | Used to build a list of pipes from a list of types.
class InitialPipes (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type]) where
  initialPipes :: IO (PipeList inputsS inputsT inputsA)

instance (InitialPipes fs as xs, Eq (f a), Show (f a)) => InitialPipes (f ': fs) (a ': as) (f a ': xs) where
  initialPipes = do
    c <- newChan :: IO (Chan (UUID, Either TaskError (f a)))
    PipeCons c <$> (initialPipes :: IO (PipeList fs as xs))

instance InitialPipes '[] '[] '[] where
  initialPipes = return PipeNil

-- | Creates the initial network used in the accumulating fold
initialNetwork
  :: forall inputsS inputsT inputsA
   . (InitialPipes inputsS inputsT inputsA)
  => IO (Network inputsS inputsT inputsA inputsS inputsT inputsA)
initialNetwork = do
  ps <- initialPipes :: IO (PipeList inputsS inputsT inputsA)
  return $ Network [] ps ps


-- | The accumulating fold to build the network.
class IFunctor7 iF => BuildNetwork iF where
  buildNetwork :: Network asS asT asA bsS bsT bsA
    -> iF Circuit bsS bsT bsA csS csT csA nbs
    -> IO (Network asS asT asA csS csT csA)

instance (BuildNetwork iF, BuildNetwork iG) => BuildNetwork (iF :+: iG) where
  buildNetwork n (L x) = buildNetwork n x
  buildNetwork n (R y) = buildNetwork n y

instance BuildNetwork Id where
  buildNetwork n Id = return n

instance BuildNetwork Task where
  buildNetwork n (Task t out) = do
    c <- newChan
    let output = PipeCons c PipeNil
    threadId <- forkIO (taskExecuter (Task t out) (outputs n) output)
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
    dupOutput :: PipeList '[f] '[a] '[f a] -> IO (PipeList '[f , f] '[a , a] '[f a , f a])
    dupOutput (PipeCons c PipeNil) = do
      c' <- dupChan c
      return $ PipeCons c (PipeCons c' PipeNil)

instance BuildNetwork Swap where
  buildNetwork n Swap = do
    output <- swapOutput (outputs n)
    return $ Network (threads n) (inputs n) output
   where
    swapOutput
      :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[g , f] '[b , a] '[g b , f a])
    swapOutput (PipeCons c1 (PipeCons c2 PipeNil)) = return $ PipeCons c2 (PipeCons c1 PipeNil)

instance BuildNetwork DropL where
  buildNetwork n DropL = do
    output <- dropLOutput (outputs n)
    return $ Network (threads n) (inputs n) output
   where
    dropLOutput :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[g] '[b] '[g b])
    dropLOutput (PipeCons _ (PipeCons c2 PipeNil)) = return $ PipeCons c2 PipeNil

instance BuildNetwork DropR where
  buildNetwork n DropR = do
    output <- dropROutput (outputs n)
    return $ Network (threads n) (inputs n) output
   where
    dropROutput :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[f] '[a] '[f a])
    dropROutput (PipeCons c1 (PipeCons _ PipeNil)) = return $ PipeCons c1 PipeNil

instance BuildNetwork Beside where
  buildNetwork = beside

beside
  :: forall asS asT asA bsS bsT bsA csS csT csA nbs
   . Network asS asT asA bsS bsT bsA
  -> Beside Circuit bsS bsT bsA csS csT csA nbs
  -> IO (Network asS asT asA csS csT csA)
beside n (Beside l r) = do
  let ninputs = circuitInputs l
  (nL  , nR  ) <- splitNetwork ninputs
  (newL, newR) <- translate ninputs (nL, nR) (l, r)
  joinNetwork (newL, newR)
 where
  splitNetwork
    :: nbsLS ~ nbsLT
    => SNat nbsL
    -> IO
         ( Network asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA)
         , Network asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA)
         )
  splitNetwork nbs = return
    ( Network (threads n) (inputs n) (takeP nbs (outputs n))
    , Network (threads n) (inputs n) (dropP nbs (outputs n))
    )

  translate
    :: SNat nbsL
    -> ( Network asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA)
       , Network asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA)
       )
    -> ( Circuit (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA) csLS csLT csLA nbsL
       , Circuit (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA) csRS csRT csRA nbsR
       )
    -> IO (Network asS asT asA csLS csLT csLA, Network asS asT asA csRS csRT csRA)
  translate _ (nL, nR) (IIn7 cL, IIn7 cR) = do
    nL' <- buildNetwork nL cL
    nR' <- buildNetwork nR cR
    return (nL', nR')

  joinNetwork
    :: (AppendP csLS csLT csLA csRS csRT csRA)
    => (Network asS asT asA csLS csLT csLA, Network asS asT asA csRS csRT csRA)
    -> IO (Network asS asT asA (csLS :++ csRS) (csLT :++ csRT) (csLA :++ csRA))
  joinNetwork (nL, nR) = return
    $ Network (nub (threads nL ++ threads nR)) (inputs nL) (outputs nL `appendP` outputs nR)


circuitInputs
  :: ( Length inputsS ~ Length inputsT
     , Length inputsT ~ Length inputsA
     , Length inputsA ~ Length inputsS
     , ninputs ~ Length inputsS
     , IsNat ninputs
     )
  => Circuit inputsS inputsT inputsA outputsS outputsT outputsA ninputs
  -> SNat (Length inputsS)
circuitInputs _ = nat

circuitInputs'
  :: ( Length bsS ~ Length bsT
     , Length bsT ~ Length bsA
     , Length bsA ~ Length bsS
     , ninputs ~ Length bsS
     , IsNat ninputs
     )
  => (N asS asT asA) bsS bsT bsA csS csT csA ninputs
  -> SNat (Length bsS)
circuitInputs' _ = nat


buildNetwork' :: forall a b c d e f (g :: Nat) . Circuit a b c d e f g -> IO (Network a b c d e f)
buildNetwork' x = case icataM7 buildNetworkAlg x of
  N f -> initialNetwork >>= f


data N asS asT asA a b c d e f (g :: Nat) = N
  { unN :: Network asS asT asA a b c -> IO (Network asS asT asA d e f)
  }

class IFunctor7 iF => BuildNetworkAlg iF where
  buildNetworkAlg :: iF (N asS asT asA) bsS bsT bsA csS csT csA (nbs :: Nat) -> (N asS asT asA) bsS bsT bsA csS csT csA (nbs :: Nat)


instance (BuildNetworkAlg iF, BuildNetworkAlg iG) => BuildNetworkAlg (iF :+: iG) where
  buildNetworkAlg (L x) = buildNetworkAlg x
  buildNetworkAlg (R y) = buildNetworkAlg y


instance BuildNetworkAlg Id where
  buildNetworkAlg Id = N return

instance BuildNetworkAlg Task where
  buildNetworkAlg (Task t out) = N
    (\n -> do
      c <- newChan
      let output = PipeCons c PipeNil
      threadId <- forkIO (taskExecuter (Task t out) (outputs n) output)
      return $ Network (threadId : threads n) (inputs n) output
    )

instance BuildNetworkAlg Replicate where
  buildNetworkAlg Replicate = N
    (\n -> do
      output <- dupOutput (outputs n)
      return $ Network (threads n) (inputs n) output
    )
   where
    dupOutput :: PipeList '[f] '[a] '[f a] -> IO (PipeList '[f , f] '[a , a] '[f a , f a])
    dupOutput (PipeCons c PipeNil) = do
      c' <- dupChan c
      return $ PipeCons c (PipeCons c' PipeNil)

instance BuildNetworkAlg Then where
  buildNetworkAlg (Then (N x) (N y)) = N
    (\n -> do
      nx <- x n
      y nx
    )

instance BuildNetworkAlg Swap where
  buildNetworkAlg Swap = N
    (\n -> do
      output <- swapOutput (outputs n)
      return $ Network (threads n) (inputs n) output
    )
   where
    swapOutput
      :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[g , f] '[b , a] '[g b , f a])
    swapOutput (PipeCons c1 (PipeCons c2 PipeNil)) = return $ PipeCons c2 (PipeCons c1 PipeNil)

instance BuildNetworkAlg DropL where
  buildNetworkAlg DropL = N
    (\n -> do
      output <- dropLOutput (outputs n)
      return $ Network (threads n) (inputs n) output
    )
   where
    dropLOutput :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[g] '[b] '[g b])
    dropLOutput (PipeCons _ (PipeCons c2 PipeNil)) = return $ PipeCons c2 PipeNil


instance BuildNetworkAlg DropR where
  buildNetworkAlg DropR = N
    (\n -> do
      output <- dropROutput (outputs n)
      return $ Network (threads n) (inputs n) output
    )
   where
    dropROutput :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[f] '[a] '[f a])
    dropROutput (PipeCons c1 (PipeCons _ PipeNil)) = return $ PipeCons c1 PipeNil


instance BuildNetworkAlg Beside where
  buildNetworkAlg = beside'

beside'
  :: forall asS asT asA bsS bsT bsA csS csT csA nbs
   . Beside (N asS asT asA) bsS bsT bsA csS csT csA nbs
  -> (N asS asT asA) bsS bsT bsA csS csT csA nbs -- IO (Network asS asT asA csS csT csA)
beside' (Beside l r) = N
  (\n -> do
    let ninputs = circuitInputs' l
    (nL  , nR  ) <- splitNetwork ninputs n
    (newL, newR) <- translate ninputs (nL, nR) (l, r)
    joinNetwork (newL, newR)
  )
 where
  splitNetwork
    :: nbsLS ~ nbsLT
    => SNat nbsL
    -> Network asS asT asA bsS bsT bsA
    -> IO
         ( Network asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA)
         , Network asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA)
         )
  splitNetwork nbs n = return
    ( Network (threads n) (inputs n) (takeP nbs (outputs n))
    , Network (threads n) (inputs n) (dropP nbs (outputs n))
    )

  translate
    :: SNat nbsL
    -> ( Network asS asT asA (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA)
       , Network asS asT asA (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA)
       )
    -> ( (N asS asT asA) (Take nbsL bsS) (Take nbsL bsT) (Take nbsL bsA) csLS csLT csLA nbsL
       , (N asS asT asA) (Drop nbsL bsS) (Drop nbsL bsT) (Drop nbsL bsA) csRS csRT csRA nbsR
       )
    -> IO (Network asS asT asA csLS csLT csLA, Network asS asT asA csRS csRT csRA)
  translate _ (nL, nR) (N cL, N cR) = do
    nL' <- cL nL
    nR' <- cR nR
    return (nL', nR')

  joinNetwork
    :: (AppendP csLS csLT csLA csRS csRT csRA)
    => (Network asS asT asA csLS csLT csLA, Network asS asT asA csRS csRT csRA)
    -> IO (Network asS asT asA (csLS :++ csRS) (csLT :++ csRT) (csLA :++ csRA))
  joinNetwork (nL, nR) = return
    $ Network (nub (threads nL ++ threads nR)) (inputs nL) (outputs nL `appendP` outputs nR)
