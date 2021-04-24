{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pipeline.Internal.Backend.Network
  ( Network(..)
  , InitialPipes(..)
  ) where

import           Control.Concurrent.Chan           (Chan, newChan)
import           Data.Kind                         (Type)
import           Pipeline.Internal.Common.HList    (HList' (..))
import           Pipeline.Internal.Core.CircuitAST (Circuit)
import           Pipeline.Internal.Core.Error      (TaskError)
import           Pipeline.Internal.Core.PipeList   (PipeList (..))
import           Pipeline.Internal.Core.UUID       (UUID)
import           Prelude                           hiding (read)

-- | Network typeclass
class Network n where
  -- | Starts a network with the given 'Circuit' as specification.
  startNetwork  :: InitialPipes inS inT inA
    => Circuit inS inT inA outS outT outA nin -- ^ The 'Circuit' used to create the network
    -> IO (n inS inT inA outS outT outA) -- ^ The created network

  -- | Stops the given network
  stopNetwork   :: n inS inT inA outS outT outA -> IO ()
  -- | This will read from the outputs of the network.
  --
  --   /This is a blocking call, therefore if there are no outputs to be read then the program will deadlock./
  read :: n inputsS inputsT inputsA outputsS outputsT outputsA -- ^ The network to retrieve inputs from
    -> IO (UUID, Either TaskError (HList' outputsS outputsT)) -- ^ The identifier for the output and the output values
  -- | Write a set of inputs into the network
  write :: UUID -- ^ A unique identifier for the input values
    -> HList' inputsS inputsT -- ^ The input values
    -> n inputsS inputsT inputsA outputsS outputsT outputsA -- ^ The network to input the values in to
    -> IO ()


-- | Used to build a list of pipes from a list of types.
class InitialPipes (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type]) where
  initialPipes :: IO (PipeList inputsS inputsT inputsA)

instance (InitialPipes fs as xs, Eq (f a), Show (f a)) => InitialPipes (f ': fs) (a ': as) (f a ': xs) where
  initialPipes = do
    c <- newChan :: IO (Chan (UUID, Either TaskError (f a)))
    PipeCons c <$> (initialPipes :: IO (PipeList fs as xs))

instance InitialPipes '[] '[] '[] where
  initialPipes = return PipeNil
