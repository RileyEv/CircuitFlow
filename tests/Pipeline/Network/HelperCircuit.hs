module Pipeline.Network.HelperCircuit where

import           Pipeline


functionTaskCircuit
  :: Circuit
       '[Var]
       '[Int]
       '[Var]
       '[Int]
       N1
functionTaskCircuit = functionTask (+ 1)

multiInputTaskCircuit
  :: Circuit
       '[Var , Var]
       '[Int , Int]
       '[Var]
       '[Int]
       N2
multiInputTaskCircuit = multiInputTask (\(HCons x (HCons y HNil)) -> x + y)
