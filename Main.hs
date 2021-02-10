{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables, TypeOperators, GADTs #-}

module Main where

import Pipeline.Core.Task (
  Task,
  VariableStore(..),
  IOStore(..),
  functionTask)

import Pipeline.Backend.GraphMachine (
  Node(..),
  Tree(..),
  processList,
  processTree)

import Data.Typeable (Typeable, cast, eqT, (:~:)(..) )

    
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
          [Tree
            (TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int IOStore String))
            []]]]]]

main :: IO ()
main = do
  _ <- processList testList (IOEmpty :: IOStore String)
  _ <- processTree testTree (IOEmpty :: IOStore String)
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
getInt x = case cast x of
  Just x1 -> x1
  Nothing -> error "error"
  
