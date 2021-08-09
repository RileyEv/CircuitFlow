module Pipeline.Network.HelperCircuit where

import           Pipeline


functionTaskCircuit
  :: IO (Circuit
       '[Var]
       '[Int]
       '[Var Int]
       '[Var]
       '[Int]
       '[Var Int]
       N1)
functionTaskCircuit = functionTask (+ 1) <$> emptyVar

multiInputTaskCircuit
  :: IO (Circuit
       '[Var , Var]
       '[Int , Int]
       '[Var Int , Var Int]
       '[Var]
       '[Int]
       '[Var Int]
       N2)
multiInputTaskCircuit = multiInputTask (\(HCons x (HCons y HNil)) -> x + y) <$> emptyVar
