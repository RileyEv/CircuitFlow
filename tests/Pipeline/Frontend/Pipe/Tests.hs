module Pipeline.Frontend.Pipe.Tests where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import Control.Monad.State (evalState, execState)
import Control.Monad.Reader (runReaderT)

import Pipeline.Frontend.Pipe
import Pipeline.Core.Task
import Pipeline.Core.DataStore

readIOTask :: Task IOStore String VariableStore Int
readIOTask = functionTask (read :: String -> Int) Empty

plus1Task :: Task VariableStore Int VariableStore Int
plus1Task = functionTask (+ (1 :: Int)) Empty

showFileTask :: FilePath -> Task VariableStore Int FileStore String
showFileTask f = functionTask (show :: Int -> String) (FileStore f)
  
replicateTask :: Task VariableStore Int VariableStore [Int]
replicateTask = functionTask (replicate 100) Empty

zipWithSelf :: FilePath -> Task VariableStore [Int] CSVStore [(Int, Int)]
zipWithSelf f = functionTask (\xs -> zip xs xs) (CSVStore f)

zipWith1To100 :: FilePath -> Task VariableStore [Int] CSVStore [(Int, Int)]
zipWith1To100 f = functionTask (zip [1..100]) (CSVStore f)

zipWith100To1 :: FilePath -> Task VariableStore [Int] CSVStore [(Int, Int)]
zipWith100To1 f = functionTask (zip [100, 99..1]) (CSVStore f)

testWorkflow1 :: Workflow Pipe
testWorkflow1 = do
  readIOTask'   <- registerTask readIOTask
  plus1Task'    <- registerTask plus1Task
  plus1Task''   <- registerTask plus1Task
  plus1Task'''  <- registerTask plus1Task
  showFileTask' <- registerTask (showFileTask "testfiles/testPipeline1.out")

  return $ Pipe $ 
    readIOTask' >>> plus1Task' >>> plus1Task'' >>> plus1Task''' >>> showFileTask' 

testWorkflow2 :: Workflow Pipe
testWorkflow2 = do
  readIOTask' <- registerTask readIOTask
  replicateTask' <- registerTask replicateTask
  zipWithSelf' <- registerTask (zipWithSelf "testfiles/testWorkflow2.1.out")
  zipWith1To100' <- registerTask (zipWith1To100 "testfiles/testWorkflow2.2.out")
  zipWith100To1' <- registerTask (zipWith100To1 "testfiles/testWorkflow2.3.out")

  return $ Pipe (readIOTask' >>> replicateTask' >>> zipWithSelf')
         & Pipe (replicateTask' >>> zipWith1To100')
         & Pipe (replicateTask' >>> zipWith100To1')

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


verifyTreeTests :: TestTree
verifyTreeTests = testGroup "verifyTree should"
  [ testCase "print types" $ do
      print "hello tests!"
      let (p, s) = buildTree testWorkflow2
      let pidTree = pipeToTree p
      runReaderT (verifyTree pidTree) (tasks s)
  ]
