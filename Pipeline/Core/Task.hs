{-# LANGUAGE MultiParamTypeClasses #-}

module Pipeline.Core.Task (
  Task(..),
  VariableStore(..),
  IOStore(..),
  DataSource(..),
  functionTask,
) where

import Data.Typeable (Typeable)


-- a i only has to be a source as it is never saved to
-- b j has to be both as this task saves to it and the next task will read

data Task f a g b = (DataSource f a, DataSource g b, Typeable f, Typeable g, Typeable a, Typeable b) => Task (f a -> g b -> IO (g b)) (g b)


class Typeable a => DataSource f a where
  fetch :: f a -> IO a
  -- First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  save :: f a -> a -> IO (f a)


-- Basic DataSource/Sink

data VariableStore a = Var a | Empty deriving Typeable

instance Typeable a => DataSource VariableStore a where
  fetch (Var x) = do
    return x
  fetch Empty   = error "empty source"
  save _ x = do
    return (Var x)


data IOStore a = IOVar a | IOEmpty deriving Typeable

instance Typeable String => DataSource IOStore String where
  fetch (IOVar x) = do
    return x
  fetch IOEmpty   = do
    putStr "Input: "
    getLine
    
  save _ x = do
    print x
    return (IOVar x)


functionTask :: (DataSource f a, DataSource g b, Typeable a, Typeable b, Typeable f, Typeable g) => (a -> b) -> g b -> Task f a g b 
functionTask f = Task (\source sink -> do
  input <- fetch source
  save sink (f input))
