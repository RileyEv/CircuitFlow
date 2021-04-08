module Main where

import Prelude hiding (id, replicate, (<>))

import Control.Monad (forM_)

import Pipeline.Circuit
import Pipeline.DataStore
import Pipeline.Nat
import Pipeline.Task
import Pipeline.Network

readIOTask :: Circuit '[IOStore] '[String] '[IOStore String] '[VariableStore] '[Int] '[VariableStore Int] N1
readIOTask = functionTask (read :: String -> Int) Empty

showFileTask :: FilePath -> Circuit '[VariableStore] '[Int] '[VariableStore Int] '[FileStore] '[String] '[FileStore String] N1
showFileTask f = functionTask (show :: Int -> String) (FileStore f)

zipWithSelf :: FilePath -> Circuit '[VariableStore] '[[Int]] '[VariableStore [Int]] '[CSVStore] '[[(Int, Int)]] '[CSVStore [(Int, Int)]] N1
zipWithSelf f = functionTask (\xs -> zip xs xs) (CSVStore f)

zipWith1To100 :: FilePath -> Circuit '[VariableStore] '[[Int]] '[VariableStore [Int]] '[CSVStore] '[[(Int, Int)]] '[CSVStore [(Int, Int)]] N1
zipWith1To100 f = functionTask (zip [1..100]) (CSVStore f)

zipWith100To1 :: FilePath -> Circuit '[VariableStore] '[[Int]] '[VariableStore [Int]] '[CSVStore] '[[(Int, Int)]] '[CSVStore [(Int, Int)]] N1
zipWith100To1 f = functionTask (zip [100, 99..1]) (CSVStore f)

plus1Task :: Circuit '[VariableStore] '[Int] '[VariableStore Int] '[VariableStore] '[Int] '[VariableStore Int] N1
plus1Task = functionTask (+1) Empty

showTask :: Circuit '[VariableStore] '[Int] '[VariableStore Int] '[VariableStore] '[String] '[VariableStore String] N1
showTask = functionTask show Empty

appendTask :: Circuit '[VariableStore, VariableStore] '[String, String] '[VariableStore String, VariableStore String]
                      '[VariableStore] '[String] '[VariableStore String] N2
appendTask = multiInputTask (\(HCons x (HCons y HNil)) -> x ++ y ) Empty


-- Example Circuits

-- Example1
--   a  
--   |  
--  +1
--   |
--  show
--   |  
--   b
example1 :: Circuit '[VariableStore] '[Int] '[VariableStore Int]
                    '[VariableStore] '[String] '[VariableStore String] N1
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
example2 :: Circuit '[VariableStore] '[Int] '[VariableStore Int]
                    '[VariableStore, VariableStore] '[String, Int] '[VariableStore String, VariableStore Int] N1
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
  '[VariableStore, VariableStore] '[String, Int] '[VariableStore String, VariableStore Int] N2
example3 = example1   <> example2
           -- VariableStore String (1), VariableStore String (2), VariableStore Int
           <-> 
           swap       <> id
           -- VariableStore String (2), VariableStore String (1), VariableStore Int
           <->
           appendTask <> id
           -- VariableStore String (2 ++ 1), VariableStore Int


main :: IO ()
main = do
  n1 <- startNetwork example2
  input_ (HCons' (Var 0) HNil') n1
  input_ (HCons' (Var 1) HNil') n1
  input_ (HCons' (Var 2) HNil') n1
  forM_ [0 .. 2] (\_ ->
    do
      (HCons' (Var x) (HCons' (Var y) HNil')) <- output_ n1
      print x
      print y)

  stopNetwork n1



-- a few rules to note...
-- replicate <-> dropL = id
-- replicate <-> dropR = id
-- replicate <-> apply f <> apply g <-> swap = replicate <-> apply g <> apply f
-- x <> y <-> swap <-> apply f = x <> y <-> apply (flip f)

-- There are probably more I will notice when using this.
-- I think example3 could be reduced so there are no swaps. Don't thing swap can be completely eliminated.
