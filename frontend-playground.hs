module Main where

import Prelude hiding (id, replicate, (<>))

import Pipeline.Circuit
import Pipeline.DataStore
import Pipeline.Nat
import Pipeline.Task
import Pipeline.Network

readIOTask :: (Task :<: iF) => IFix7 iF '[IOStore] '[String] '[IOStore String] '[VariableStore] '[Int] '[VariableStore Int] ('Succ 'Zero)
readIOTask = functionTask (read :: String -> Int) Empty

showFileTask :: (Task :<: iF) => FilePath -> IFix7 iF '[VariableStore] '[Int] '[VariableStore Int] '[FileStore] '[String] '[FileStore String] ('Succ 'Zero)
showFileTask f = functionTask (show :: Int -> String) (FileStore f)

-- replicateTask :: Task '[VariableStore] '[Int] VariableStore [Int]
-- replicateTask = functionTask (replicate 100) Empty

zipWithSelf :: (Task :<: iF) => FilePath -> IFix7 iF '[VariableStore] '[[Int]] '[VariableStore [Int]] '[CSVStore] '[[(Int, Int)]] '[CSVStore [(Int, Int)]] ('Succ 'Zero)
zipWithSelf f = functionTask (\xs -> zip xs xs) (CSVStore f)

zipWith1To100 :: (Task :<: iF) => FilePath -> IFix7 iF '[VariableStore] '[[Int]] '[VariableStore [Int]] '[CSVStore] '[[(Int, Int)]] '[CSVStore [(Int, Int)]] ('Succ 'Zero)
zipWith1To100 f = functionTask (zip [1..100]) (CSVStore f)

zipWith100To1 :: (Task :<: iF) => FilePath -> IFix7 iF '[VariableStore] '[[Int]] '[VariableStore [Int]] '[CSVStore] '[[(Int, Int)]] '[CSVStore [(Int, Int)]] ('Succ 'Zero)
zipWith100To1 f = functionTask (zip [100, 99..1]) (CSVStore f)


-- Some example tasks
plus1Task :: (Task :<: iF) => IFix7 iF '[VariableStore] '[Int] '[VariableStore Int] '[VariableStore] '[Int] '[VariableStore Int] ('Succ 'Zero)
plus1Task = functionTask (+1) Empty

showTask :: (Task :<: iF) => IFix7 iF '[VariableStore] '[Int] '[VariableStore Int] '[VariableStore] '[String] '[VariableStore String] ('Succ 'Zero)
showTask = functionTask show Empty

appendTask :: (Task :<: iF)
  => IFix7 iF '[VariableStore, VariableStore] '[String, String] '[VariableStore String, VariableStore String]
              '[VariableStore] '[String] '[VariableStore String] ('Succ ('Succ 'Zero))
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
                    '[VariableStore] '[String] '[VariableStore String] ('Succ 'Zero)
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
                    '[VariableStore, VariableStore] '[String, Int] '[VariableStore String, VariableStore Int] ('Succ 'Zero)
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
  '[VariableStore, VariableStore] '[String, Int] '[VariableStore String, VariableStore Int] ('Succ ('Succ 'Zero))
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
  input (HCons' (Var 0) HNil') n1
  input (HCons' (Var 1) HNil') n1
  input (HCons' (Var 2) HNil') n1
  (HCons' (Var x) (HCons' (Var y) HNil')) <- output n1
  print x
  print y
  (HCons' (Var x) (HCons' (Var y) HNil')) <- output n1
  print x
  print y
  (HCons' (Var x) (HCons' (Var y) HNil')) <- output n1
  print x
  print y

  stopNetwork n1



-- a few rules to note...
-- replicate <-> dropL = id
-- replicate <-> dropR = id
-- replicate <-> apply f <> apply g <-> swap = replicate <-> apply g <> apply f
-- x <> y <-> swap <-> apply f = x <> y <-> apply (flip f)

-- There are probably more I will notice when using this.
-- I think example3 could be reduced so there are no swaps. Don't thing swap can be completely eliminated.
