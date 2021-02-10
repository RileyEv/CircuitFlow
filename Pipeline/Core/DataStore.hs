{-# LANGUAGE MultiParamTypeClasses #-}

module Pipeline.Core.DataStore (
  DataSource(..),
  VariableStore(..),
  IOStore(..),
  FileStore(..),
  CSVStore(..)
) where

import Data.Typeable (Typeable)

class Typeable a => DataSource f a where
  -- | Fetch the value stored in the 'DataSource'
  fetch :: f a -> IO a
  -- | Save a value into the 'DataStore'
  --   First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  save :: f a -> a -> IO (f a)


{-|
  A 'VariableStore' is a simple in memory 'DataStore'.
-}
data VariableStore a = Var a | Empty deriving Typeable

instance Typeable a => DataSource VariableStore a where
  fetch (Var x) = do
    return x
  fetch Empty   = error "empty source"
  save _ x = do
    return (Var x)


{-|
  An 'IOStore' is a simple in memory 'DataStore', with some extra features.
  Fetching from an empty store will read input from stdin and writing to a
  store will cause the output to be writo to stdout.
-}
data IOStore a = IOVar a | IOEmpty deriving Typeable

{-|
  An instance in only defined for String types
-}
instance Typeable String => DataSource IOStore String where
  fetch (IOVar x) = do
    return x
  fetch IOEmpty   = do
    putStr "Input: "
    getLine
    
  save _ x = do
    print x
    return (IOVar x)


newtype FileStore a = FileStore String deriving Typeable

instance Typeable String => DataSource FileStore String where
  fetch (FileStore fname) = do
    return ""

  save f@(FileStore fname) x = do
    return f



-- CSV Store

newtype CSVStore a = CSVStore String deriving Typeable

instance Typeable a => DataSource CSVStore a where
  fetch (CSVStore fname) = undefined
  save f@(CSVStore fname) = undefined
