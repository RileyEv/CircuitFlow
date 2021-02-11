{-# LANGUAGE MultiParamTypeClasses #-}

module Pipeline.Core.DataStore (
  DataSource(..),
  VariableStore(..),
  IOStore(..),
  FileStore(..),
  CSVStore(..)
) where

import Data.Typeable (Typeable)
import Data.Csv (encode, decode, ToRecord, FromRecord, HasHeader(..))
import qualified Data.ByteString.Lazy as B (readFile, writeFile)
import qualified Data.Vector as V (toList)

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

{-|
  A 'FileStore' is able to write a string to a file for intermediate
  between tasks
-}
newtype FileStore a = FileStore String deriving Typeable

instance Typeable String => DataSource FileStore String where
  fetch (FileStore fname) = readFile fname
  save f@(FileStore fname) x = do
    writeFile fname x
    return f

-- splits file into lines
instance Typeable [String] => DataSource FileStore [String] where
  fetch (FileStore fname) = do
    f <- readFile fname
    return (lines f)
  save f@(FileStore fname) x = do
    let x' = unlines x
    writeFile fname x'
    return f


-- CSV Store

newtype CSVStore a = CSVStore String deriving Typeable

instance (Typeable [a], ToRecord a, FromRecord a) => DataSource CSVStore [a] where
  fetch (CSVStore fname) = do
    f <- B.readFile fname
    let dec = decode NoHeader f
        x' = case dec of
          Right x -> x
          Left err -> error err
    return (V.toList x')
    
  save f@(CSVStore fname) x = do
    let enc = encode x
    B.writeFile fname enc
    return f
