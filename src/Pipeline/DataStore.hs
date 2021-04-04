module Pipeline.DataStore (
  module Pipeline.Internal.Core.DataStore,
  VariableStore(..),
  IOStore(..),
  FileStore(..),
  CSVStore(..)
) where


import Pipeline.Internal.Core.DataStore (DataStore(..))

import Data.Csv (encode, decode, ToRecord, FromRecord, HasHeader(..))
import qualified Data.ByteString.Lazy as B (readFile, writeFile)
import qualified Data.Vector as V (toList)


{-|
  A 'VariableStore' is a simple in memory 'DataStore'.
-}
data VariableStore a = Var a | Empty deriving (Eq, Show)

instance DataStore VariableStore a where
  fetch (Var x) = return x
  fetch Empty   = error "empty source"
  save _ x = return (Var x)


{-|
  An 'IOStore' is a simple in memory 'DataStore', with some extra features.
  Fetching from an empty store will read input from stdin and writing to a
  store will cause the output to be writo to stdout.
-}
data IOStore a = IOVar a | IOEmpty deriving (Eq, Show)

{-|
  An instance in only defined for String types
-}
instance DataStore IOStore String where
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
newtype FileStore a = FileStore String deriving (Eq, Show)

{-|
  You are able to write a String to a FileStore.
-}
instance DataStore FileStore String where
  fetch (FileStore fname) = readFile fname
  save f@(FileStore fname) x = do
    writeFile fname x
    return f

{-|
  It is possible to write a list of strings to a 'FileStore'.
  A new line is added between each string in the list.
-}
instance DataStore FileStore [String] where
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
newtype CSVStore a = CSVStore String deriving (Eq, Show)

{-|
  A list of any type can be wrote to a CSV as long as it has a 'ToRecord'
  and 'FromRecord' instance defined.
-}
instance (ToRecord a, FromRecord a) => DataStore CSVStore [a] where
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
