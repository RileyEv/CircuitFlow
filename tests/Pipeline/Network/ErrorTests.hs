module Pipeline.Network.ErrorTests
  ( errorTests
  ) where

import           Control.Exception.Lifted       (ArithException (..), throwIO)
import           Control.Monad.Trans            (lift)
import           Pipeline
import           Pipeline.Network.HelperCircuit
import           Prelude                        hiding (id, replicate, (<>))
import           Test.Tasty
import           Test.Tasty.HUnit

errorTests :: TestTree
errorTests =
  testGroup "Tests that shouldn't throw a run-time exception" [divBy0Tests, errorPropagationTests]

{-| Helper function to create a network from the given Circuit
    and then input a value and recieve the output
-}
singleInputTest
  :: (InitialPipes a b c)
  => Circuit a b c d e f g
  -> HList' a b
  -> IO (Either TaskError (HList' d e))
singleInputTest circuit i = do
  n <- startNetwork circuit
  input_ i n
  out <- output_ n
  stopNetwork n
  return out


-- Tests for the 'Task' constructor
divByZeroCircuit
  :: Circuit
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       N1
divByZeroCircuit = functionTask (`div` 0) Empty

divByZeroCircuit'
  :: Circuit
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       N1
divByZeroCircuit' = task
  (\_ _ _ -> do
    _ <- lift (throwIO DivideByZero)
    return (Var 0)
  )
  Empty

divBy0Tests :: TestTree
divBy0Tests =
  let i = HCons' (Var 10) HNil'
  in  testGroup
        "dividing by zero should"
        [ testCase "throw an error" $ helper i divByZeroCircuit
        , testCase "throw an error" $ helper i divByZeroCircuit'
        ]
 where
  helper
    :: HList' '[VariableStore] '[Int]
    -> Circuit
         '[VariableStore]
         '[Int]
         '[VariableStore Int]
         '[VariableStore]
         '[Int]
         '[VariableStore Int]
         N1
    -> IO ()
  helper i c = do
    o <- singleInputTest c i
    o @?= Left (TaskError (ExceptionMessage "divide by zero"))


faultyCircuit
  :: Circuit
       '[VariableStore , VariableStore]
       '[Int , Int]
       '[VariableStore Int , VariableStore Int]
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       N2
faultyCircuit = divByZeroCircuit <> functionTaskCircuit <-> multiInputTaskCircuit

errorPropagationTests :: TestTree
errorPropagationTests = testGroup
  "an error should"
  [ testCase "propagate through the network" $ do
      let i = HCons' (Var 0) (HCons' (Var 1) HNil')
      o <- singleInputTest faultyCircuit i
      o @?= Left (TaskError (ExceptionMessage "divide by zero"))
  ]
