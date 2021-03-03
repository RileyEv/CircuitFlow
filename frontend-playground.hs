module Main where

import Pipeline (runWorkflow)
import Pipeline.Frontend.Pipe (Pipe(..), (>>>), (&))
import Pipeline.Frontend.Workflow (Workflow, registerTask)
import Pipeline.Core.Task (Task, functionTask)
import Pipeline.Core.Node (extractLeafs)
import Pipeline.Core.DataStore (IOStore(..), VariableStore(..), FileStore(..), CSVStore(..))


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


main :: IO ()
main = do
  _ <- runWorkflow testWorkflow1 (IOEmpty :: IOStore String)
  _ <- runWorkflow testWorkflow2 (IOEmpty :: IOStore String)
  return ()
