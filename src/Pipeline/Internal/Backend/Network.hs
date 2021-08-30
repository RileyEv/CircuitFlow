{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pipeline.Internal.Backend.Network
  ( Network(..)
  , InitialPipes(..)
  , N(..)
  , BuildNetworkAlg(..)
  ) where

import           Control.Concurrent.Chan                   (Chan, newChan)
import           Data.Kind                                 (Type)
import           Pipeline.Internal.Common.HList            (HList' (..))
import           Pipeline.Internal.Common.IFunctor         (IFunctor5)
import           Pipeline.Internal.Common.IFunctor.Modular ((:+:) (..))
import           Pipeline.Internal.Core.CircuitAST         (Circuit)
import           Pipeline.Internal.Core.Error              (TaskError)
import           Pipeline.Internal.Core.PipeList           (PipeList (..))
import           Pipeline.Internal.Core.UUID               (JobUUID)
import           Prelude                                   hiding (read)

-- | Network typeclass
class Network n where
  -- | Starts a network with the given 'Circuit' as specification.
  startNetwork  :: InitialPipes inS inT
    => Circuit inS inT outS outT nin -- ^ The 'Circuit' used to create the network
    -> IO (n inS inT outS outT) -- ^ The created network

  -- | Stops the given network
  stopNetwork   :: n inS inT outS outT -> IO ()
  -- | This will read from the outputs of the network.
  --
  --   /This is a blocking call, therefore if there are no outputs to be read then the program will deadlock./
  read :: n inputsS inputsT outputsS outputsT -- ^ The network to retrieve inputs from
    -> IO (JobUUID, Either TaskError (HList' outputsS outputsT)) -- ^ The identifier for the output and the output values
  -- | Write a set of inputs into the network
  write :: JobUUID -- ^ A unique identifier for the input values
    -> HList' inputsS inputsT -- ^ The input values
    -> n inputsS inputsT outputsS outputsT -- ^ The network to input the values in to
    -> IO ()


-- | Used to build a list of pipes from a list of types.
class InitialPipes (inputsS :: [Type -> Type]) (inputsT :: [Type]) where
  initialPipes :: IO (PipeList inputsS inputsT)

instance (InitialPipes fs as, Eq (f a)) => InitialPipes (f ': fs) (a ': as) where
  initialPipes = do
    c <- newChan :: IO (Chan (JobUUID, Either TaskError (f a)))
    PipeCons c <$> (initialPipes :: IO (PipeList fs as))

instance InitialPipes '[] '[]  where
  initialPipes = return PipeNil


newtype N n asS asT a b c d e = N
  { unN :: n asS asT a b -> IO (n asS asT c d)
  }

-- | The accumulating fold to build the network.
class (IFunctor5 iF, Network n) => BuildNetworkAlg n iF where
  buildNetworkAlg :: iF (N n asS asT) bsS bsT csS csT nbs -> IO ((N n asS asT) bsS bsT csS csT nbs)


instance (BuildNetworkAlg n iF, BuildNetworkAlg n iG) => BuildNetworkAlg n (iF :+: iG) where
  buildNetworkAlg (L x) = buildNetworkAlg x
  buildNetworkAlg (R y) = buildNetworkAlg y
