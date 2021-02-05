{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
module Pipeline.Core.Task (
  Task,
  DataSource(..),
  DataSink(..),
  VariableStore(..),
  IOStore(..),
  functionTask
) where

import Data.Typeable (Typeable(..))


class Monad m => DataSource m a i where
  fetch :: a i -> m i

class Monad m => DataSink m a i where
  save :: a i -> i -> m (a i)

type Task i b j = forall m a. (
  Monad m, DataSource m a i, DataSink m b j,
  Typeable i, Typeable j, Typeable m, Typeable b, Typeable a) => a i -> b j -> m (b j)


-- Some Basic DataSources and DataSinks

-- VariableStore will store a value that is passed to another task.
data VariableStore a = Var a | Empty deriving Typeable

instance Monad m => DataSource m VariableStore a where
  fetch (Var x) = return x
  fetch Empty = error "empty source"

instance Monad m => DataSink m VariableStore a where
  save _ x = return (Var x)


-- IOStore is able to interact with the world.
data IOStore a = IOIn | IOOut

-- The DataSource will read in a line of text from input
instance DataSource IO IOStore String where
  fetch IOIn = do
    putStr "Input: "
    getLine
  fetch IOOut = error "cant fetch from sink"

-- The DataSink is able to print the result to the console as long as a Show instance exists
instance Show a => DataSink IO IOStore a where
  save _ x = do
    print x
    return IOOut


functionTask :: (i -> j) -> Task i b j 
functionTask f source sink = do
  input <- fetch source
  save sink (f input)
