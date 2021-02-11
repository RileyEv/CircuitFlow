{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables, TypeOperators, GADTs #-}

module Main where

import Pipeline.Core.Task (
  Task,
  functionTask)

import Pipeline.Backend.GraphMachine (
  Node(..),
  Tree(..),
  processList,
  processTree)

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
testTree :: Tree Node
testTree = Tree 
  (TaskNode (functionTask (++ ("00" :: String)) Empty :: Task IOStore String VariableStore String))
  [Tree
    (TaskNode (functionTask (read :: String -> Int) Empty :: Task VariableStore String VariableStore Int))
    [Tree
      (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
      [Tree
        (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
        [Tree
          (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
          [Tree (TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int IOStore String)) []]]]]]

-- read in a string add two 0s to the end and then print it out 3 times.
testTree2 :: Tree Node
testTree2 = Tree
  (TaskNode (functionTask (++ ("00" :: String)) Empty :: Task IOStore String VariableStore String))
  [Tree
    (TaskNode (functionTask (read :: String -> Int) Empty :: Task VariableStore String VariableStore Int))
    [Tree (TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int IOStore String)) [],
     Tree (TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int IOStore String)) [],
     Tree (TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int IOStore String)) []]]

-- same as 'testTree', but uses a file instead of stdin/out.
testTree3 :: Tree Node
testTree3 = Tree 
    (TaskNode (functionTask (read :: String -> Int) Empty :: Task FileStore String VariableStore Int))
    [Tree
      (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
      [Tree
        (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
        [Tree
          (TaskNode (functionTask (+ (1 :: Int)) Empty :: Task VariableStore Int VariableStore Int))
          [Tree (TaskNode (functionTask (show :: Int -> String) (FileStore "testfiles/testTree3.out") :: Task VariableStore Int FileStore String)) []]]]]

main :: IO ()
main = do
  -- _ <- processList testList  (IOEmpty :: IOStore String)
  -- _ <- processTree testTree  (IOEmpty :: IOStore String)
  -- _ <- processTree testTree2 (IOEmpty :: IOStore String)
  _ <- processTree testTree3 (FileStore "testfiles/testTree3.in" :: FileStore String)
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

