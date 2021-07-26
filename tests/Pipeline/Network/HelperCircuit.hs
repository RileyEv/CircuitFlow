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
functionTaskCircuit = do
  var <- emptyVar
  return $ functionTask (+ 1) var

multiInputTaskCircuit
  :: IO (Circuit
       '[Var , Var]
       '[Int , Int]
       '[Var Int , Var Int]
       '[Var]
       '[Int]
       '[Var Int]
       N2)
multiInputTaskCircuit = do
  var <- emptyVar
  return $ multiInputTask (\(HCons x (HCons y HNil)) -> x + y) var
