module Pipeline.Frontend.Pipe.Tests where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import Control.Monad.State (evalState, execState)

import Pipeline.Frontend.Pipe

tests :: TestTree
tests = testGroup "Frontend" [ nextPIDTests
                             ]


nextPIDTests :: TestTree
nextPIDTests = testGroup "nextPID should"
  [ testCase "return the next pid" $ do
      let pid = evalState nextPID (WorkflowState 3 M.empty)
      pid @?= 4
  , testCase "increment the counter in state" $ do
      let (WorkflowState pid _) = execState nextPID (WorkflowState 5 M.empty)
      pid @?= 6
  ]


extractLeafsTests :: TestTree
extractLeafsTests = testGroup "extractLeafs should"
  [ testCase "return " $ do return ()]
