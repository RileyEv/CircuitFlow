{-|
Module      : Pipeline.DataStore
Description : The DataStore class and some pre-defined stores.
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

This package contains the 'DataStore' class, it is the method used to transferring data between tasks.
-}
module Pipeline.DataStore
  (
  -- * The DataStore Class
    DataStore(..)
  ,
  -- ** Combined DataStores
    DataStore'(..)
  ,
  -- * Pre-Defined DataStores
  -- ** Var
    Var
  , emptyVar
  ,
  -- ** FileStore
    FileStore(..)
  ,
  -- ** CSVStore
    CSVStore(..)
  , NamedCSVStore(..)
  -- ** Defining DataStores
  , Generic
  , NFData
  ) where


import           Pipeline.Internal.Core.DataStore (DataStore (..),
                                                   DataStore' (..), Var, emptyVar)
import           Pipeline.Internal.Core.UUID      (JobUUID)
import           Pipeline.Internal.Backend.FileGen (createNewFile)

import           Control.DeepSeq                  (NFData)
import qualified Data.ByteString.Lazy             as B (readFile, writeFile)
import           Data.Csv                         (DefaultOrdered,
                                                   FromNamedRecord, FromRecord,
                                                   HasHeader (..),
                                                   ToNamedRecord, ToRecord,
                                                   decode, decodeByName, encode,
                                                   encodeDefaultOrderedByName)
import qualified Data.Vector                      as V (toList)
import           GHC.Generics                     (Generic)
import           System.FilePath                  (splitFileName, (</>))


{-|
  A 'FileStore' is able to write a string to a file for intermediate
  between tasks
-}
newtype FileStore a = FileStore FilePath deriving (Eq, Show, Generic, NFData)

{-|
  You are able to write a String to a FileStore.
-}
instance DataStore FileStore String where
  fetch (FileStore fname) = readFile fname
  save (FileStore fname) x = do
    writeFile fname x
  empty taskUUID jobUUID = FileStore <$> createNewFile taskUUID jobUUID ".txt"

{-|
  It is possible to write a list of strings to a 'FileStore'.
  A new line is added between each string in the list.
-}
instance DataStore FileStore [String] where
  fetch (FileStore fname) = do
    f <- readFile fname
    return (lines f)
  save (FileStore fname) x = do
    let x'     = unlines x
    writeFile fname x'
  empty taskUUID jobUUID = FileStore <$> createNewFile taskUUID jobUUID ".txt"


{-|
  A 'CSVStore' is able to write data to a csv file.
-}
newtype CSVStore a = CSVStore FilePath deriving (Eq, Show, Generic, NFData)

{-|
  A list of any type can be wrote to a CSV as long as it has a 'ToRecord'
  and 'FromRecord' instance defined.
-}
instance (ToRecord a, FromRecord a) => DataStore CSVStore [a] where
  fetch (CSVStore fname) = do
    f <- B.readFile fname
    let dec = decode NoHeader f
        x'  = case dec of
          Right x   -> x
          Left  err -> error err
    return (V.toList x')
  save (CSVStore fname) x = do
    let enc    = encode x
    B.writeFile fname enc
  empty taskUUID jobUUID = CSVStore <$> createNewFile taskUUID jobUUID ".csv"

{-|
  A 'NamedCSVStore' is able to write data to a csv file, with a header.
-}
newtype NamedCSVStore a = NamedCSVStore FilePath deriving (Eq, Show, Generic, NFData)

{-|
  A list of any type can be wrote to a CSV as long as it has a 'ToNamedRecord',
 'FromNamedRecord', and 'DefaultOrdered' instance defined.
-}
instance (ToNamedRecord a, FromNamedRecord a, DefaultOrdered a) => DataStore NamedCSVStore [a] where
  fetch (NamedCSVStore fname) = do
    f <- B.readFile fname
    let dec = decodeByName f
        x'  = case dec of
          Right (_, x) -> x
          Left  err    -> error err
    return (V.toList x')
  save (NamedCSVStore fname) x = do
    let enc    = encodeDefaultOrderedByName x
    B.writeFile fname enc
  empty taskUUID jobUUID = NamedCSVStore <$> createNewFile taskUUID jobUUID ".csv"
