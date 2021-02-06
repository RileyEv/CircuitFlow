{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances, GADTs, KindSignatures, DataKinds, FlexibleContexts #-}
module Pipeline.Core.Task (
  Task(..),
  VariableStore(..),
  DataSource(..),
  DataSink(..),
  Node(..),
  processGraph,
  testGraph,
  functionTask,
) where

import Data.Typeable (Typeable(..), cast, gcast, eqT, (:~:)(..))


-- a i only has to be a source as it is never saved to
-- b j has to be both as this task saves to it and the next task will read

data Task a b = (DataSource VariableStore a, DataSink VariableStore b, DataSource VariableStore b, Typeable a, Typeable b) => Task (VariableStore a -> VariableStore b -> IO (VariableStore b))



class Typeable a => DataSource f a where
  fetch :: f a -> IO a

class Typeable a => DataSink f a where
  -- First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  save :: f a -> a -> IO (f a) 

getInt :: Typeable a => a -> Int
getInt x = case cast x of
  Just x1 -> x1
  Nothing -> error "error"

-- Basic DataSource/Sink

data VariableStore a = Var a | Empty

instance Typeable a => DataSource VariableStore a where
  fetch (Var x) = do
    -- print (getInt x)
    return x
  fetch Empty   = error "empty source"

instance Typeable a => DataSink VariableStore a where
  save _ x = do
    -- print (getInt x)
    return (Var x)




data Node = forall a b. (Typeable a, Typeable b) =>  TaskNode (Task a b)
          | forall a. (Typeable a, DataSource VariableStore a) => DataNode (VariableStore a)
-- data Node = forall i b j. (Typeable b, Typeable i, Typeable j) =>  TaskNode (Task i b j)
--           | forall a i. (DataSource a i, Typeable a, Typeable i) => DataNode (a i)


-- Lets start with a simple graph represented with a list. ie a -> b -> .. -> x
processGraph :: (DataSource VariableStore a, Typeable a) => [Node] -> VariableStore a -> IO [Node]
processGraph ns firstD = do
  arr <- foldl f (return [DataNode firstD]) ns
  return (tail arr)
  where
    f :: IO [Node] -> Node -> IO [Node]
    f ds' t = do
      ds <- ds'
      let dn = last ds
          nextd' = test dn t
      nextd <- nextd'
      return (ds ++ [nextd])


test :: Node -> Node -> IO Node
test (DataNode d) (TaskNode (Task t)) = do
  r <- applyTask d (Task t)
  return (DataNode r)
test _ _ = error ""

applyTask :: (Typeable a, Typeable b, Typeable c) => VariableStore a -> Task b c -> IO (VariableStore c)
applyTask d (Task t) = case gcast d of
  Just d1 -> t d1 Empty
  Nothing -> error ""


testGraph :: [Node]
testGraph = [
  TaskNode (functionTask (++ ("00" :: String))),
  TaskNode (functionTask (read :: String -> Int)),
  TaskNode (functionTask (+ (1 :: Int))),
  TaskNode (functionTask (+ (1 :: Int))),
  TaskNode (functionTask (+ (1 :: Int)))]

functionTask :: (Typeable a, Typeable b) => (a -> b) -> Task a b 
functionTask f = Task (\source sink -> do
  input <- fetch source
  save sink (f input))
