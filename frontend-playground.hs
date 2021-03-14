module Main where

import Pipeline.Frontend.Circuit

import Pipeline.Core.Task (Task, functionTask, multiInputFunctionTask)
import Pipeline.Core.DataStore (IOStore(..), VariableStore(..), FileStore(..), CSVStore(..), HList(..))

import Prelude hiding (id, replicate, (<>))


readIOTask :: Task '[IOStore] '[String] VariableStore Int
readIOTask = functionTask (read :: String -> Int) Empty

showFileTask :: FilePath -> Task '[VariableStore] '[Int] FileStore String
showFileTask f = functionTask (show :: Int -> String) (FileStore f)

-- replicateTask :: Task '[VariableStore] '[Int] VariableStore [Int]
-- replicateTask = functionTask (replicate 100) Empty

zipWithSelf :: FilePath -> Task '[VariableStore] '[[Int]] CSVStore [(Int, Int)]
zipWithSelf f = functionTask (\xs -> zip xs xs) (CSVStore f)

zipWith1To100 :: FilePath -> Task '[VariableStore] '[[Int]] CSVStore [(Int, Int)]
zipWith1To100 f = functionTask (zip [1..100]) (CSVStore f)

zipWith100To1 :: FilePath -> Task '[VariableStore] '[[Int]] CSVStore [(Int, Int)]
zipWith100To1 f = functionTask (zip [100, 99..1]) (CSVStore f)


-- Some example tasks
plus1Task :: Task '[VariableStore] '[Int] VariableStore Int
plus1Task = functionTask (+1) Empty

showTask :: Task '[VariableStore] '[Int] VariableStore String
showTask = functionTask show Empty

appendTask :: Task '[VariableStore, VariableStore] '[String, String] VariableStore String
appendTask = multiInputFunctionTask (\(HCons x (HCons y HNil)) -> x ++ y ) Empty


-- Example Circuits

-- Example1
--   a  
--   |  
--  +1
--   |
--  show
--   |  
--   b
example1 :: Circuit '[VariableStore Int] '[VariableStore String]
example1 = id
          <->
          apply plus1Task
          <->
          apply showTask


-- Example 2
--     a
--    / \
--   +1 show
--    \ /
--     /
--    / \
--    | |
--    b c
example2 :: Circuit '[VariableStore Int] '[VariableStore String, VariableStore Int]
example2 = replicate
           <->
           apply plus1Task <> apply showTask
           <->
           swap

-- Example 3
--   a      b
--   |     / \
--  +1    +1 show
--  show   \ /
--   |      /
--   |     / \
--   |    /  |
--   |   /   |
--   |  /    |
--   \ /     | 
--    /      |
--   / \     |
--   \ /     |
--   ++      |
--    |      |
--    c      d
example3 :: Circuit '[VariableStore Int, VariableStore Int] '[VariableStore String, VariableStore Int]
example3 = example1         <> example2
           -- VariableStore String (1), VariableStore String (2), VariableStore Int
           <-> 
           swap             <> id
           -- VariableStore String (2), VariableStore String (1), VariableStore Int
           <->
           apply appendTask <> id
           -- VariableStore String (2 ++ 1), VariableStore Int


-- a few rules to note...
-- replicate <-> dropL = id
-- replicate <-> dropR = id
-- replicate <-> apply f <> apply g <-> swap = replicate <-> apply g <> apply f
-- x <> y <-> swap <-> apply f = x <> y <-> apply (flip f)

-- There are probably more I will notice when using this.
-- I think example3 could be reduced so there are no swaps. Don't thing swap can be completely eliminated.
