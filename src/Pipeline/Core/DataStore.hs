{-# LANGUAGE MultiParamTypeClasses #-}

module Pipeline.Core.DataStore (
  DataSource(..),
  DataWrap(..),
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

data DataWrap = forall f a. (DataSource f a, Typeable f, Typeable a) => DataWrap (f a)

{-|
  A 'VariableStore' is a simple in memory 'DataStore'.
-}
data VariableStore a = Var a | Empty deriving Typeable

instance Typeable a => DataSource VariableStore a where
  fetch (Var x) = return x
  fetch Empty   = error "empty source"
  save _ x = return (Var x)


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
  fetch (IOVar x) = return x
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

{-|
  You are able to write a String to a FileStore.
-}
instance Typeable String => DataSource FileStore String where
  fetch (FileStore fname) = readFile fname
  save f@(FileStore fname) x = do
    writeFile fname x
    return f

{-|
  It is possible to write a list of strings to a 'FileStore'.
  A new line is added between each string in the list.
-}
instance Typeable [String] => DataSource FileStore [String] where
  fetch (FileStore fname) = do
    f <- readFile fname
    return (lines f)
  save f@(FileStore fname) x = do
    let x' = unlines x
    writeFile fname x'
    return f


{-|
  A 'CSVStore' is able to write data to a csv file.
-}
newtype CSVStore a = CSVStore String deriving Typeable

{-|
  A list of any type can be wrote to a CSV as long as it has a 'ToRecord'
  and 'FromRecord' instance defined.
-}
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
