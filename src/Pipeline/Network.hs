{-|
Module      : Pipeline.Network
Description : To create and control process networks
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

Contains the tools needed to create, interact with and stop a network.
-}
module Pipeline.Network
  (
  -- * Network
    Network
  , startNetwork
  , stopNetwork
  ,
  -- * Network IO
    UUID
  ,
  -- ** Input
    input
  , input_
  , inputUUID
  ,
  -- ** Output
    output
  , output_
  , module Pipeline.Internal.Common.HList
  ,
  -- * Misc
    InitialPipes
  ) where

import           Pipeline.Internal.Backend.ProcessNetwork (Network, inputUUID,
                                                           output, stopNetwork)
import           Pipeline.Internal.Backend.Translation    (InitialPipes,
                                                           buildNetwork,
                                                           initialNetwork)
import           Pipeline.Internal.Common.HList           (HList' (..))
import           Pipeline.Internal.Common.IFunctor        (IFix7 (..))
import           Pipeline.Internal.Core.CircuitAST        (Circuit)
import           Pipeline.Internal.Core.UUID              (UUID, genUUID)


-- | This will create a new 'Network' for the given 'Circuit'
startNetwork
  :: InitialPipes inputsS inputsT inputsA
  => Circuit inputsS inputsT inputsA outputsS outputsT outputsA ninputs -- ^ The 'Circuit' used to create the network
  -> IO (Network inputsS inputsT inputsA outputsS outputsT outputsA) -- ^ The created network
startNetwork (IIn7 c) = do
  n <- initialNetwork
  buildNetwork n c

-- | Input values into a network.
-- This will return a randomly generated identifier for the inputs.
input
  :: HList' inputsS inputsT -- ^ Inputs to the network
  -> Network inputsS inputsT inputsA outputsS outputsT outputsA -- ^ Network to insert the values in
  -> IO UUID -- ^ Randomly generated identifier
input x n = do
  uuid <- genUUID
  inputUUID uuid x n
  return uuid

-- | A variant of 'input', however it will not return the randomly generated identifier.
input_
  :: HList' inputsS inputsT
  -> Network inputsS inputsT inputsA outputsS outputsT outputsA
  -> IO ()
input_ x n = do
  _ <- input x n
  return ()


-- | A variant of 'output', that does not return the unique identifier.
--
--   /This is a blocking call, therefore if there are no outputs to be read then the program will deadlock./
output_
  :: Network inputsS inputsT inputsA outputsS outputsT outputsA
  -> IO (HList' outputsS outputsT)
output_ n = output n >>= (\(_, x) -> return x)
