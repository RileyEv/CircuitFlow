{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables, GADTs #-}

module Main where

import Pipeline.Core.Task (Task, TaskWrap(..), functionTask)
import Pipeline.Core.Node (Node(..))
import Pipeline.Core.Graph (TreeF(..), TaskTreeF(..), FList(..))
import Pipeline.Core.IFunctor (IFix(..), IFix2(..))
import Pipeline.Core.DataStore (
  VariableStore(..),
  IOStore(..),
  FileStore(..),
  CSVStore(..),
  HList(..))
import Pipeline.Backend.GraphMachine (processTree)


    
testList :: [Node]
testList = [
  TaskNode $ TaskWrap  (functionTask (++ ("00" :: String))   Empty  :: Task '[IOStore]       '[String] VariableStore String),
  TaskNode $ TaskWrap (functionTask (read :: String -> Int) Empty   :: Task '[VariableStore] '[String] VariableStore Int),
  TaskNode $ TaskWrap (functionTask (+ (1 :: Int))          Empty   :: Task '[VariableStore] '[Int]    VariableStore Int),
  TaskNode $ TaskWrap (functionTask (+ (1 :: Int))          Empty   :: Task '[VariableStore] '[Int]    VariableStore Int),
  TaskNode $ TaskWrap (functionTask (+ (1 :: Int))          Empty   :: Task '[VariableStore] '[Int]    VariableStore Int),
  TaskNode $ TaskWrap (functionTask (show :: Int -> String) IOEmpty :: Task '[VariableStore] '[Int]    IOStore       String)]

-- Has same semantics as `testList`
testTree :: (IFix TreeF) Node
testTree = IIn $ TreeF 
  (TaskNode $ TaskWrap (functionTask (++ ("00" :: String)) Empty :: Task '[IOStore] '[String] VariableStore String))
  [IIn $ TreeF
    (TaskNode $ TaskWrap (functionTask (read :: String -> Int) Empty :: Task '[VariableStore] '[String] VariableStore Int))
    [IIn $ TreeF
      (TaskNode $ TaskWrap (functionTask (+ (1 :: Int)) Empty :: Task '[VariableStore] '[Int] VariableStore Int))
      [IIn $ TreeF
        (TaskNode $ TaskWrap (functionTask (+ (1 :: Int)) Empty :: Task '[VariableStore] '[Int] VariableStore Int))
        [IIn $ TreeF
          (TaskNode $ TaskWrap (functionTask (+ (1 :: Int)) Empty :: Task '[VariableStore] '[Int] VariableStore Int))
          [IIn $ TreeF (TaskNode $ TaskWrap (functionTask (show :: Int -> String) IOEmpty :: Task '[VariableStore] '[Int] IOStore String)) []]]]]]

-- read in a string add two 0s to the end and then print it out 3 times.
testTree2 :: (IFix TreeF) Node
testTree2 = IIn $ TreeF
  (TaskNode $ TaskWrap (functionTask (++ ("00" :: String)) Empty :: Task '[IOStore] '[String] VariableStore String))
  [IIn $ TreeF
    (TaskNode $ TaskWrap (functionTask (read :: String -> Int) Empty :: Task '[VariableStore] '[String] VariableStore Int))
    [IIn $ TreeF (TaskNode $ TaskWrap (functionTask (show :: Int -> String) IOEmpty :: Task '[VariableStore] '[Int] IOStore String)) [],
     IIn $ TreeF (TaskNode $ TaskWrap (functionTask (show :: Int -> String) IOEmpty :: Task '[VariableStore] '[Int] IOStore String)) [],
     IIn $ TreeF (TaskNode $ TaskWrap (functionTask (show :: Int -> String) IOEmpty :: Task '[VariableStore] '[Int] IOStore String)) []]]

-- same as 'testTree', but uses a file instead of stdin/out.
testTree3 :: (IFix TreeF) Node
testTree3 = IIn $ TreeF 
    (TaskNode $ TaskWrap (functionTask (read :: String -> Int) Empty :: Task '[FileStore] '[String] VariableStore Int))
    [IIn $ TreeF
      (TaskNode $ TaskWrap (functionTask (+ (1 :: Int)) Empty :: Task '[VariableStore] '[Int] VariableStore Int))
      [IIn $ TreeF
        (TaskNode $ TaskWrap (functionTask (+ (1 :: Int)) Empty :: Task '[VariableStore] '[Int] VariableStore Int))
        [IIn $ TreeF
          (TaskNode $ TaskWrap (functionTask (+ (1 :: Int)) Empty :: Task '[VariableStore] '[Int] VariableStore Int))
          [IIn $ TreeF (TaskNode $ TaskWrap (functionTask (show :: Int -> String) (FileStore "testfiles/testTree3.out") :: Task '[VariableStore] '[Int] FileStore String)) []]]]]

testTree4 :: (IFix TreeF) Node
testTree4 = IIn $ TreeF
  (TaskNode $ TaskWrap (functionTask (read :: String -> Int) Empty :: Task '[IOStore] '[String] VariableStore Int))
  [IIn $ TreeF
    (TaskNode $ TaskWrap (functionTask (replicate 100 :: Int -> [Int]) Empty :: Task '[VariableStore] '[Int] VariableStore [Int]))
    [IIn $ TreeF (TaskNode $ TaskWrap (functionTask ((\xs -> zip xs xs) :: [Int] -> [(Int, Int)]) (CSVStore "testfiles/testTree4.1.out") :: Task '[VariableStore] '[[Int]] CSVStore [(Int, Int)])) [],
     IIn $ TreeF (TaskNode $ TaskWrap (functionTask (zip [1..100]       :: [Int] -> [(Int, Int)]) (CSVStore "testfiles/testTree4.2.out") :: Task '[VariableStore] '[[Int]] CSVStore [(Int, Int)])) [],
     IIn $ TreeF (TaskNode $ TaskWrap (functionTask (zip [100, 99..1]   :: [Int] -> [(Int, Int)]) (CSVStore "testfiles/testTree4.3.out") :: Task '[VariableStore] '[[Int]] CSVStore [(Int, Int)])) []]]


add00Task :: Task '[IOStore] '[String] VariableStore String
add00Task = functionTask (++ ("00" :: String)) Empty

readTask :: Task '[VariableStore] '[String] VariableStore Int
readTask = functionTask (read :: String -> Int) Empty

showTask :: Task '[VariableStore] '[Int] IOStore String
showTask = functionTask (show :: Int -> String) IOEmpty

newTree :: (IFix2 TaskTreeF) '[IOStore String] '[IOStore String, IOStore String, IOStore String]
newTree = IIn2 $ TBranchF add00Task (FCons (
            IIn2 $ TBranchF readTask (FCons (
              IIn2 $ TLeafF showTask) (FCons (
              IIn2 $ TLeafF showTask) (FCons (
              IIn2 $ TLeafF showTask) FNil)))) FNil)



main :: IO ()
main = do
  -- _ <- processList testList  (IOEmpty :: IOStore String)
  -- _ <- processTree testTree  (IOEmpty :: IOStore String)
  -- _ <- processTree testTree2 (IOEmpty :: IOStore String)
  -- _ <- processTree testTree3 (FileStore "testfiles/testTree3.in" :: FileStore String)
  _ <- processTree testTree4 (HCons IOEmpty HNil :: HList '[IOStore String])
  return ()


