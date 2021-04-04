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
  input,
  output,
  module Pipeline.Internal.Common.HList,
  -- * Misc
  InitialPipes,
) where

import Pipeline.Internal.Core.CircuitAST (Circuit)
import Pipeline.Internal.Common.IFunctor (IFix7(..))
import Pipeline.Internal.Common.HList (HList'(..))
import Pipeline.Internal.Backend.ProcessNetwork (Network, stopNetwork, input, output)
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
