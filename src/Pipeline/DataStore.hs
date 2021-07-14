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
  -- ** VariableStore
  -- ** IOStore
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
                                                   DataStore' (..))
import           Pipeline.Internal.Core.UUID      (UUID)

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



-- {-|
--   An 'IOStore' is a simple in memory 'DataStore', with some extra features.

--   Fetching from an empty store will read input from stdin and writing to a
--   store will cause the output to be wrote to stdout.
-- -}
-- data IOStore a = IOVar a | IOEmpty deriving (Eq, Show, Generic, NFData)

-- {-|
--   An instance is only defined for String types
-- -}
-- instance DataStore IOStore String where
--   fetch _ (IOVar x) = return x
--   fetch _ IOEmpty   = do
--     putStr "Input: "
--     getLine

--   save _ _ x = do
--     print x
--     return (IOVar x)

addUUIDToFileName :: String -> UUID -> String
addUUIDToFileName fpath uuid =
  let (directory, fname) = splitFileName fpath in directory </> uuid ++ "-" ++ fname


{-|
  A 'FileStore' is able to write a string to a file for intermediate
  between tasks
-}
newtype FileStore a = FileStore String deriving (Eq, Show, Generic, NFData)

{-|
  You are able to write a String to a FileStore.
-}
instance DataStore FileStore String where
  fetch _ (FileStore fname) = readFile fname
  save uuid (FileStore fname) x = do
    let fname' = addUUIDToFileName fname uuid
    writeFile fname' x

{-|
  It is possible to write a list of strings to a 'FileStore'.
  A new line is added between each string in the list.
-}
instance DataStore FileStore [String] where
  fetch _ (FileStore fname) = do
    f <- readFile fname
    return (lines f)
  save uuid (FileStore fname) x = do
    let x'     = unlines x
        fname' = addUUIDToFileName fname uuid
    writeFile fname' x'


{-|
  A 'CSVStore' is able to write data to a csv file.
-}
newtype CSVStore a = CSVStore String deriving (Eq, Show, Generic, NFData)

{-|
  A list of any type can be wrote to a CSV as long as it has a 'ToRecord'
  and 'FromRecord' instance defined.
-}
instance (ToRecord a, FromRecord a) => DataStore CSVStore [a] where
  fetch _ (CSVStore fname) = do
    f <- B.readFile fname
    let dec = decode NoHeader f
        x'  = case dec of
          Right x   -> x
          Left  err -> error err
    return (V.toList x')

  save uuid (CSVStore fname) x = do
    let enc    = encode x
        fname' = addUUIDToFileName fname uuid
    B.writeFile fname' enc

{-|
  A 'NamedCSVStore' is able to write data to a csv file, with a header.
-}
newtype NamedCSVStore a = NamedCSVStore String deriving (Eq, Show, Generic, NFData)

{-|
  A list of any type can be wrote to a CSV as long as it has a 'ToNamedRecord',
 'FromNamedRecord', and 'DefaultOrdered' instance defined.
-}
instance (ToNamedRecord a, FromNamedRecord a, DefaultOrdered a) => DataStore NamedCSVStore [a] where
  fetch _ (NamedCSVStore fname) = do
    f <- B.readFile fname
    let dec = decodeByName f
        x'  = case dec of
          Right (_, x) -> x
          Left  err    -> error err
    return (V.toList x')

  save uuid (NamedCSVStore fname) x = do
    let enc    = encodeDefaultOrderedByName x
        fname' = addUUIDToFileName fname uuid
    B.writeFile fname' enc
