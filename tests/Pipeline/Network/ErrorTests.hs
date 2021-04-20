module Pipeline.Network.ErrorTests
  ( errorTests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Prelude                  hiding (id, replicate, (<>))

import           Control.Exception.Lifted (ArithException (..),
                                           SomeException (..), throwIO)
import           Control.Monad.Trans      (lift)
import           Pipeline

errorTests :: TestTree
errorTests =
  testGroup "Tests that shouldn't throw a run-time exception" [divBy0Tests', divBy0Tests]

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
functionTaskCircuit
  :: Circuit
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       N1
functionTaskCircuit = functionTask (`div` 0) Empty

divBy0Tests :: TestTree
divBy0Tests = testGroup
  "functionTask should"
  [ testCase "apply the function to the input value" $ do
      let i = HCons' (Var 10) HNil'
      o <- singleInputTest functionTaskCircuit i
      print o
      o @?= Left (TaskError (ExceptionMessage "divide by zero"))
  ]

functionTaskCircuit'
  :: Circuit
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       '[VariableStore]
       '[Int]
       '[VariableStore Int]
       N1
functionTaskCircuit' = task
  (\uuid inputs out -> do
    _ <- lift (throwIO DivideByZero)
    return (Var 0)
  )
  Empty

divBy0Tests' :: TestTree
divBy0Tests' = testGroup
  "functionTask should"
  [ testCase "apply the function to the input value" $ do
      let i = HCons' (Var 10) HNil'
      o <- singleInputTest functionTaskCircuit' i
      print o
      o @?= Left (TaskError (ExceptionMessage "divide by zero"))
  ]
