module Pipeline.Network.HelperCircuit where

import           Pipeline


functionTaskCircuit
  :: Circuit
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       N1
functionTaskCircuit = functionTask (+ 1) Empty

multiInputTaskCircuit
  :: Circuit
       '[VariableStore , VariableStore]
       '[Int , Int]
       '[VariableStore Int , VariableStore Int]
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       N2
multiInputTaskCircuit = multiInputTask (\(HCons x (HCons y HNil)) -> x + y) Empty
