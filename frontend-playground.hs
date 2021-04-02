module Main where

import Pipeline.Frontend.Circuit

import Pipeline.Core.Task (TaskF, functionTaskF, multiInputFunctionTaskF)
import Pipeline.Core.HList
import Pipeline.Core.DataStore (IOStore(..), VariableStore(..), FileStore(..), CSVStore(..))
import Pipeline.Core.Modular ((:<:)(..))
import Pipeline.Core.IFunctor (IFix6) 

import Prelude hiding (id, replicate, (<>))


readIOTask :: (TaskF :<: iF) => IFix6 iF '[IOStore] '[String] '[IOStore String] '[VariableStore] '[Int] '[VariableStore Int]
readIOTask = functionTaskF (read :: String -> Int) Empty

showFileTask :: (TaskF :<: iF) => FilePath -> IFix6 iF '[VariableStore] '[Int] '[VariableStore Int] '[FileStore] '[String] '[FileStore String]
showFileTask f = functionTaskF (show :: Int -> String) (FileStore f)

-- replicateTask :: Task '[VariableStore] '[Int] VariableStore [Int]
-- replicateTask = functionTask (replicate 100) Empty

zipWithSelf :: (TaskF :<: iF) => FilePath -> IFix6 iF '[VariableStore] '[[Int]] '[VariableStore [Int]] '[CSVStore] '[[(Int, Int)]] '[CSVStore [(Int, Int)]]
zipWithSelf f = functionTaskF (\xs -> zip xs xs) (CSVStore f)

zipWith1To100 :: (TaskF :<: iF) => FilePath -> IFix6 iF '[VariableStore] '[[Int]] '[VariableStore [Int]] '[CSVStore] '[[(Int, Int)]] '[CSVStore [(Int, Int)]]
zipWith1To100 f = functionTaskF (zip [1..100]) (CSVStore f)

zipWith100To1 :: (TaskF :<: iF) => FilePath -> IFix6 iF '[VariableStore] '[[Int]] '[VariableStore [Int]] '[CSVStore] '[[(Int, Int)]] '[CSVStore [(Int, Int)]]
zipWith100To1 f = functionTaskF (zip [100, 99..1]) (CSVStore f)


-- Some example tasks
plus1Task :: (TaskF :<: iF) => IFix6 iF '[VariableStore] '[Int] '[VariableStore Int] '[VariableStore] '[Int] '[VariableStore Int]
plus1Task = functionTaskF (+1) Empty

showTask :: (TaskF :<: iF) => IFix6 iF '[VariableStore] '[Int] '[VariableStore Int] '[VariableStore] '[String] '[VariableStore String]
showTask = functionTaskF show Empty

appendTask :: (TaskF :<: iF)
  => IFix6 iF '[VariableStore, VariableStore] '[String, String] '[VariableStore String, VariableStore String] '[VariableStore] '[String] '[VariableStore String]
appendTask = multiInputFunctionTaskF (\(HCons x (HCons y HNil)) -> x ++ y ) Empty


-- Example Circuits

-- Example1
--   a  
--   |  
--  +1
--   |
--  show
--   |  
--   b
example1 :: Circuit '[VariableStore] '[Int] '[VariableStore Int] '[VariableStore] '[String] '[VariableStore String]
example1 = id
          <->
          plus1Task
          <->
          showTask


-- Example 2
--     a
--    / \
--   +1 show
--    \ /
--     /
--    / \
--    | |
--    b c
example2 :: Circuit '[VariableStore] '[Int] '[VariableStore Int] '[VariableStore, VariableStore] '[String, Int] '[VariableStore String, VariableStore Int]
example2 = replicate
           <->
           plus1Task <> showTask
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
example3 :: Circuit
  '[VariableStore, VariableStore] '[Int, Int] '[VariableStore Int, VariableStore Int]
  '[VariableStore, VariableStore] '[String, Int] '[VariableStore String, VariableStore Int]
example3 = example1   <> example2
           -- VariableStore String (1), VariableStore String (2), VariableStore Int
           <-> 
           swap       <> id
           -- VariableStore String (2), VariableStore String (1), VariableStore Int
           <->
           appendTask <> id
           -- VariableStore String (2 ++ 1), VariableStore Int


-- a few rules to note...
-- replicate <-> dropL = id
-- replicate <-> dropR = id
-- replicate <-> apply f <> apply g <-> swap = replicate <-> apply g <> apply f
-- x <> y <-> swap <-> apply f = x <> y <-> apply (flip f)

-- There are probably more I will notice when using this.
-- I think example3 could be reduced so there are no swaps. Don't thing swap can be completely eliminated.
