{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances, GADTs, KindSignatures, DataKinds, FlexibleContexts, TypeOperators, PartialTypeSignatures, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fprint-typechecker-elaboration #-}

module Pipeline.Core.Task (
  Task(..),
  VariableStore(..),
  IOStore(..),
  DataSource(..),
  Node(..),
  processGraph,
  testGraph,
  functionTask,
) where

import Data.Typeable (Typeable(..), cast, gcast, eqT, (:~:)(..))


-- a i only has to be a source as it is never saved to
-- b j has to be both as this task saves to it and the next task will read

data Task f a g b = (DataSource f a, DataSource g b, Typeable f, Typeable g, Typeable a, Typeable b) => Task (f a -> g b -> IO (g b)) (g b)


class Typeable a => DataSource f a where
  fetch :: f a -> IO a
  -- First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  save :: f a -> a -> IO (f a)


getInt :: Typeable a => a -> Int
getInt x = case cast x of
  Just x1 -> x1
  Nothing -> error "error"

-- Basic DataSource/Sink

data VariableStore a = Var a | Empty deriving Typeable

instance Typeable a => DataSource VariableStore a where
  fetch (Var x) = do
    -- print (getInt x)
    return x
  fetch Empty   = error "empty source"
  save _ x = do
    -- print (getInt x)
    return (Var x)


data IOStore a = IOVar a | IOEmpty deriving Typeable

instance Typeable String => DataSource IOStore String where
  fetch (IOVar x) = do
    -- print (getInt x)
    return x
  fetch IOEmpty   = do
    putStr "Input: "
    getLine
    
  save _ x = do
    print x
    return (IOVar x)




data Node = forall f a g b. (Typeable f, Typeable g, Typeable a, Typeable b, DataSource f a, DataSource g b) => TaskNode (Task f a g b)
          | forall f a. (Typeable f, Typeable a, DataSource f a) => DataNode (f a)
-- data Node = forall i b j. (Typeable b, Typeable i, Typeable j) =>  TaskNode (Task i b j)
--           | forall a i. (DataSource a i, Typeable a, Typeable i) => DataNode (a i)


-- Lets start with a simple graph represented with a list. ie a -> b -> .. -> x
processGraph :: (DataSource f a, Typeable a, Typeable f) => [Node] -> f a -> IO [Node]
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
test (DataNode d) (TaskNode t) = do
  r <- applyFCast d t 
  return (DataNode r)
test _ _ = error ""


applyFCast :: forall f g h a b c. (Typeable f, Typeable g, Typeable h, Typeable a, Typeable b, Typeable c) => f a -> Task g b h c -> IO (h c)
applyFCast d (Task t o) = case (eqT :: Maybe (f :~: g)) of
  Just Refl -> applyTask d t o
  Nothing -> error ""
  


applyTask :: (Typeable f, Typeable h, Typeable a, Typeable b, Typeable c) => f a -> (f b -> h c -> IO (h c)) -> h c -> IO (h c)
applyTask d t o = case gcast d of
  Just d1 -> t d1 o
  Nothing -> error ""


testGraph :: [Node]
testGraph = [
  TaskNode (functionTask (++ ("00" :: String))   Empty   :: Task IOStore       String VariableStore String),
  TaskNode (functionTask (read :: String -> Int) Empty   :: Task VariableStore String VariableStore Int),
  TaskNode (functionTask (+ (1 :: Int))          Empty   :: Task VariableStore Int    VariableStore Int),
  TaskNode (functionTask (+ (1 :: Int))          Empty   :: Task VariableStore Int    VariableStore Int),
  TaskNode (functionTask (+ (1 :: Int))          Empty   :: Task VariableStore Int    VariableStore Int),
  TaskNode (functionTask (show :: Int -> String) IOEmpty :: Task VariableStore Int    IOStore       String)]

functionTask :: (DataSource f a, DataSource g b, Typeable a, Typeable b, Typeable f, Typeable g) => (a -> b) -> g b -> Task f a g b 
functionTask f = Task (\source sink -> do
  input <- fetch source
  save sink (f input))
  
