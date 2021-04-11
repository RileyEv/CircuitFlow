{-|
Module      : Pipeline.Network
Description : To create and control process networks
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

Contains the tools needed to create, interact with and stop a network.
-}
module Pipeline.Network (
  -- * Network
  Network,
  startNetwork,
  stopNetwork,
  -- * Network IO
  UUID,
  input,
  input_,
  inputUUID,
  output,
  output_,
  module Pipeline.Internal.Common.HList,
  -- * Misc
  InitialPipes,
) where

import Pipeline.Internal.Core.CircuitAST (Circuit)
import Pipeline.Internal.Core.UUID (UUID, genUUID)
import Pipeline.Internal.Common.IFunctor (IFix7(..))
import Pipeline.Internal.Common.HList (HList'(..))
import Pipeline.Internal.Backend.ProcessNetwork (Network, stopNetwork, inputUUID, output)
import Pipeline.Internal.Backend.Translation (buildNetwork, InitialPipes, initialNetwork)


-- | This will create a new 'Network' for the given 'Circuit'
startNetwork :: InitialPipes inputsS inputsT inputsA
  -- | The 'Circuit' used to create the network
  => Circuit inputsS inputsT inputsA outputsS outputsT outputsA ninputs
  -- | The created network
  -> IO (Network inputsS inputsT inputsA outputsS outputsT outputsA)
startNetwork (IIn7 c) = do
  n <- initialNetwork
  buildNetwork n c



-- | This will write the given input to the network 
input_ :: HList' inputsS inputsT -> Network inputsS inputsT inputsA outputsS outputsT outputsA -> IO ()
input_ x n = do
  _ <- input x n
  return ()

input :: HList' inputsS inputsT -> Network inputsS inputsT inputsA outputsS outputsT outputsA -> IO UUID
input x n = do
  uuid <- genUUID
  inputUUID uuid x n
  return uuid



-- | This will read from the outputs of the network.
--
--   This is a blocking call, therefore if there are no outputs to be read then the program will deadlock.
output_ :: Network inputsS inputsT inputsA outputsS outputsT outputsA -> IO (HList' outputsS outputsT)
output_ n = output n >>= (\(_, x) -> return x)
