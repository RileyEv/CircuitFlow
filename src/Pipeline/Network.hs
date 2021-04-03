module Pipeline.Network (
  module Pipeline.Internal.Backend.ProcessNetwork,
  module Pipeline.Internal.Common.HList,
  startNetwork
) where

import Pipeline.Internal.Core.CircuitAST (Circuit)
import Pipeline.Internal.Common.IFunctor (IFix7(..))
import Pipeline.Internal.Common.HList (HList'(..))
import Pipeline.Internal.Backend.ProcessNetwork (Network, stopNetwork, input, output)
import Pipeline.Internal.Backend.Translation (buildNetwork, InitialPipes, initialNetwork)


startNetwork :: InitialPipes inputsS inputsT inputsA
  => Circuit inputsS inputsT inputsA outputsS outputsT outputsA ninputs
  -> IO (Network inputsS inputsT inputsA outputsS outputsT outputsA)
startNetwork (IIn7 c) = do
  n <- initialNetwork
  buildNetwork n c
