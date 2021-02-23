{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables, TypeOperators, GADTs #-}

module Main where

import Pipeline.Core.Task (
  Task,
  functionTask)

import Pipeline.Backend.GraphMachine (
  Node(..),
  TreeF(..),
  processList,
  processTree)

import Pipeline.Core.IFunctor (IFix(..))

import Pipeline.Core.DataStore (
  VariableStore(..),
  IOStore(..),
  FileStore(..),
  CSVStore(..))

import Data.Typeable (Typeable, cast, eqT, (:~:)(..) )
import Data.Maybe (fromMaybe)

    
testList :: [Node]
testList = [
  TaskNode (functionTask (++ ("00" :: String))   Empty   :: Task IOStore       String VariableStore String),
  TaskNode (functionTask (read :: String -> Int) Empty   :: Task VariableStore String VariableStore Int),
  TaskNode (functionTask (+ (1 :: Int))          Empty   :: Task VariableStore Int    VariableStore Int),
  TaskNode (functionTask (+ (1 :: Int))          Empty   :: Task VariableStore Int    VariableStore Int),
  TaskNode (functionTask (+ (1 :: Int))          Empty   :: Task VariableStore Int    VariableStore Int),
  TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int    IOStore       String)]

-- Has same semantics as `testList`
testTree :: (IFix TreeF) Node
testTree = IIn $ TreeF 
  (TaskNode (functionTask (++ ("00" :: String)) Empty :: Task IOStore String VariableStore String))
  [IIn $ TreeF
    (TaskNode (functionTask (read :: String -> Int) Empty :: Task VariableStore String VariableStore Int))
    [IIn $ TreeF
      (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
      [IIn $ TreeF
        (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
        [IIn $ TreeF
          (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
          [IIn $ TreeF (TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int IOStore String)) []]]]]]

-- read in a string add two 0s to the end and then print it out 3 times.
testTree2 :: (IFix TreeF) Node
testTree2 = IIn $ TreeF
  (TaskNode (functionTask (++ ("00" :: String)) Empty :: Task IOStore String VariableStore String))
  [IIn $ TreeF
    (TaskNode (functionTask (read :: String -> Int) Empty :: Task VariableStore String VariableStore Int))
    [IIn $ TreeF (TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int IOStore String)) [],
     IIn $ TreeF (TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int IOStore String)) [],
     IIn $ TreeF (TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int IOStore String)) []]]

-- same as 'testTree', but uses a file instead of stdin/out.
testTree3 :: (IFix TreeF) Node
testTree3 = IIn $ TreeF 
    (TaskNode (functionTask (read :: String -> Int) Empty :: Task FileStore String VariableStore Int))
    [IIn $ TreeF
      (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
      [IIn $ TreeF
        (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
        [IIn $ TreeF
          (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
          [IIn $ TreeF (TaskNode (functionTask (show :: Int -> String) (FileStore "testfiles/testTree3.out") :: Task VariableStore Int FileStore String)) []]]]]

testTree4 :: (IFix TreeF) Node
testTree4 = IIn $ TreeF
  (TaskNode (functionTask (read :: String -> Int) Empty :: Task IOStore String VariableStore Int))
  [IIn $ TreeF
    (TaskNode (functionTask (replicate 100 :: Int -> [Int]) Empty :: Task VariableStore Int VariableStore [Int]))
    [IIn $ TreeF (TaskNode (functionTask ((\xs -> zip xs xs) :: [Int] -> [(Int, Int)]) (CSVStore "testfiles/testTree4.1.out") :: Task VariableStore [Int] CSVStore [(Int, Int)])) [],
     IIn $ TreeF (TaskNode (functionTask (zip [1..100]       :: [Int] -> [(Int, Int)]) (CSVStore "testfiles/testTree4.2.out") :: Task VariableStore [Int] CSVStore [(Int, Int)])) [],
     IIn $ TreeF (TaskNode (functionTask (zip [100, 99..1]   :: [Int] -> [(Int, Int)]) (CSVStore "testfiles/testTree4.3.out") :: Task VariableStore [Int] CSVStore [(Int, Int)])) []]]

main :: IO ()
main = do
  -- _ <- processList testList  (IOEmpty :: IOStore String)
  -- _ <- processTree testTree  (IOEmpty :: IOStore String)
  -- _ <- processTree testTree2 (IOEmpty :: IOStore String)
  -- _ <- processTree testTree3 (FileStore "testfiles/testTree3.in" :: FileStore String)
  _ <- processTree testTree4 (IOEmpty :: IOStore String)
  return ()

test :: Node -> IO Int
test (DataNode d) = test'' d
test _ = error ""

test'' :: forall f a. (Typeable f, Typeable a) => f a -> IO Int
test'' d = case eqT :: Maybe (VariableStore :~: f) of
  Just Refl -> test' d
  Nothing -> error ""

test' :: Typeable a => VariableStore a -> IO Int
test' (Var x) = case cast x of
  Just x1 -> return x1
  Nothing -> error ""
test' _ = error ""

getInt :: Typeable a => a -> Int
getInt x = fromMaybe (error "error") (cast x)

