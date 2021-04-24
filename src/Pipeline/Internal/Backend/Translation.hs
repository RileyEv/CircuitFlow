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
import           Pipeline.Internal.Common.IFunctor         (IFunctor7, icataM7)
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

circuitInputs
  :: ( Length bsS ~ Length bsT
     , Length bsT ~ Length bsA
     , Length bsA ~ Length bsS
     , ninputs ~ Length bsS
     , IsNat ninputs
     )
  => (N asS asT asA) bsS bsT bsA csS csT csA (ninputs :: Nat)
  -> SNat (Length bsS)
circuitInputs _ = nat


buildNetwork :: InitialPipes a b c => Circuit a b c d e f g -> IO (Network a b c d e f)
buildNetwork x = do
  n  <- icataM7 buildNetworkAlg x
  n' <- initialNetwork
  unN n n'


newtype N asS asT asA a b c d e f g = N
  { unN :: Network asS asT asA a b c -> IO (Network asS asT asA d e f)
  }

-- | The accumulating fold to build the network.
class IFunctor7 iF => BuildNetworkAlg iF where
  buildNetworkAlg :: iF (N asS asT asA) bsS bsT bsA csS csT csA (nbs :: Nat) -> IO ((N asS asT asA) bsS bsT bsA csS csT csA (nbs :: Nat))


instance (BuildNetworkAlg iF, BuildNetworkAlg iG) => BuildNetworkAlg (iF :+: iG) where
  buildNetworkAlg (L x) = buildNetworkAlg x
  buildNetworkAlg (R y) = buildNetworkAlg y


instance BuildNetworkAlg Id where
  buildNetworkAlg Id = return (N return)

instance BuildNetworkAlg Task where
  buildNetworkAlg (Task t out) = return $ N
    (\n -> do
      c <- newChan
      let output = PipeCons c PipeNil
      threadId <- forkIO (taskExecuter (Task t out) (outputs n) output)
      return $ Network (threadId : threads n) (inputs n) output
    )

instance BuildNetworkAlg Replicate where
  buildNetworkAlg Replicate = return $ N
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
  buildNetworkAlg (Then (N x) (N y)) = return $ N
    (\n -> do
      nx <- x n
      y nx
    )

instance BuildNetworkAlg Swap where
  buildNetworkAlg Swap = return $ N
    (\n -> do
      output <- swapOutput (outputs n)
      return $ Network (threads n) (inputs n) output
    )
   where
    swapOutput
      :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[g , f] '[b , a] '[g b , f a])
    swapOutput (PipeCons c1 (PipeCons c2 PipeNil)) = return $ PipeCons c2 (PipeCons c1 PipeNil)

instance BuildNetworkAlg DropL where
  buildNetworkAlg DropL = return $ N
    (\n -> do
      output <- dropLOutput (outputs n)
      return $ Network (threads n) (inputs n) output
    )
   where
    dropLOutput :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[g] '[b] '[g b])
    dropLOutput (PipeCons _ (PipeCons c2 PipeNil)) = return $ PipeCons c2 PipeNil


instance BuildNetworkAlg DropR where
  buildNetworkAlg DropR = return $ N
    (\n -> do
      output <- dropROutput (outputs n)
      return $ Network (threads n) (inputs n) output
    )
   where
    dropROutput :: PipeList '[f , g] '[a , b] '[f a , g b] -> IO (PipeList '[f] '[a] '[f a])
    dropROutput (PipeCons c1 (PipeCons _ PipeNil)) = return $ PipeCons c1 PipeNil


instance BuildNetworkAlg Beside where
  buildNetworkAlg = beside

beside
  :: forall asS asT asA bsS bsT bsA csS csT csA (nbs :: Nat)
   . Beside (N asS asT asA) bsS bsT bsA csS csT csA nbs
  -> IO ((N asS asT asA) bsS bsT bsA csS csT csA nbs) -- IO (Network asS asT asA csS csT csA)
beside (Beside l r) = return $ N
  (\n -> do
    let ninputs = circuitInputs l
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
