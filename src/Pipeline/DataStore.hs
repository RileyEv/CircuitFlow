{-|
Module      : Pipeline.DataStore
Description : The DataStore class and some pre-defined stores.
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

This package contains the 'DataStore' class, it is the method used to transferring data between tasks.
-}
module Pipeline.DataStore (
  -- * The DataStore Class
  DataStore(..),
  -- ** Combined DataStores
  DataStore'(..),
  -- * Pre-Defined DataStores
  -- ** VariableStore
  VariableStore(..),
  -- ** IOStore
  IOStore(..),
  -- ** FileStore
  FileStore(..),
  -- ** CSVStore
  CSVStore(..),
  NamedCSVStore(..),
) where


import Pipeline.Internal.Core.UUID (UUID)
import Pipeline.Internal.Core.DataStore (DataStore'(..), DataStore(..))

import Data.Csv (
  encode, encodeDefaultOrderedByName, decode, decodeByName,
  ToRecord, FromRecord, HasHeader(..), ToNamedRecord, FromNamedRecord, DefaultOrdered)
import qualified Data.ByteString.Lazy as B (readFile, writeFile)
import qualified Data.Vector as V (toList, Vector)


{-|
  A 'VariableStore' is a simple in memory 'DataStore'.
-}
data VariableStore a = Var a | Empty deriving (Eq, Show)

instance DataStore VariableStore a where
  fetch _ (Var x) = return x
  fetch _ Empty   = error "empty source"
  save _ _ x = return (Var x)


{-|
  An 'IOStore' is a simple in memory 'DataStore', with some extra features.

  Fetching from an empty store will read input from stdin and writing to a
  store will cause the output to be wrote to stdout.
-}
data IOStore a = IOVar a | IOEmpty deriving (Eq, Show)

{-|
  An instance is only defined for String types
-}
instance DataStore IOStore String where
  fetch _ (IOVar x) = return x
  fetch _ IOEmpty   = do
    putStr "Input: "
    getLine
    
  save _ _ x = do
    print x
    return (IOVar x)

addUUIDToFileName :: String -> UUID -> String
addUUIDToFileName fname uuid = uuid ++ "-" ++ fname


{-|
  A 'FileStore' is able to write a string to a file for intermediate
  between tasks
-}
newtype FileStore a = FileStore String deriving (Eq, Show)

{-|
  You are able to write a String to a FileStore.
-}
instance DataStore FileStore String where
  fetch uuid (FileStore fname) = readFile (addUUIDToFileName fname uuid)
  save uuid f@(FileStore fname) x = do
    writeFile (addUUIDToFileName fname uuid) x
    return f

{-|
  It is possible to write a list of strings to a 'FileStore'.
  A new line is added between each string in the list.
-}
instance DataStore FileStore [String] where
  fetch uuid (FileStore fname) = do
    f <- readFile (addUUIDToFileName fname uuid)
    return (lines f)
  save uuid f@(FileStore fname) x = do
    let x' = unlines x
    writeFile (addUUIDToFileName fname uuid) x'
    return f


{-|
  A 'CSVStore' is able to write data to a csv file.
-}
newtype CSVStore a = CSVStore String deriving (Eq, Show)

{-|
  A list of any type can be wrote to a CSV as long as it has a 'ToRecord'
  and 'FromRecord' instance defined.
-}
instance (ToRecord a, FromRecord a) => DataStore CSVStore [a] where
  fetch uuid (CSVStore fname) = do
    f <- B.readFile (addUUIDToFileName fname uuid)
    let dec = decode NoHeader f
        x' = case dec of
          Right x -> x
          Left err -> error err
    return (V.toList x')
    
  save uuid f@(CSVStore fname) x = do
    let enc = encode x
    B.writeFile (addUUIDToFileName fname uuid) enc
    return f

{-|
  A 'NamedCSVStore' is able to write data to a csv file.
-}
newtype NamedCSVStore a = NamedCSVStore String deriving (Eq, Show)

{-|
  A list of any type can be wrote to a CSV as long as it has a 'ToRecord'
  and 'FromRecord' instance defined.
-}
instance (ToNamedRecord a, FromNamedRecord a, DefaultOrdered a) => DataStore NamedCSVStore [a] where
  fetch uuid (NamedCSVStore fname) = do
    f <- B.readFile (addUUIDToFileName fname uuid)
    let dec = decodeByName f
        x'  = case dec of
          Right (_, x) -> x
          Left err -> error err
    return (V.toList x')
    
  save uuid f@(NamedCSVStore fname) x = do
    let enc = encodeDefaultOrderedByName x
    B.writeFile (addUUIDToFileName fname uuid) enc
    return f
