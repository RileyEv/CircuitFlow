{-# LANGUAGE MultiParamTypeClasses, DataKinds, PolyKinds, TypeFamilyDependencies, ExplicitNamespaces #-}

module Pipeline.Core.DataStore (
  DataSource(..),
  DataSource'(..),
  IOList(..),
  HList(..),
  type (++),
  Apply,
  DataWrap(..),
  VariableStore(..),
  IOStore(..),
  FileStore(..),
  CSVStore(..)
) where

import Pipeline.Core.HList (HList(..))

import Data.Typeable (Typeable)
import Data.Csv (encode, decode, ToRecord, FromRecord, HasHeader(..))
import qualified Data.ByteString.Lazy as B (readFile, writeFile)
import qualified Data.Vector as V (toList)

-- DataSource that can be defined for each datastore to be used.
class Typeable a => DataSource f a where
  -- | Fetch the value stored in the 'DataSource'
  fetch :: f a -> IO a
  -- | Save a value into the 'DataStore'
  --   First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  save :: f a -> a -> IO (f a)

-- Magic wrapper for a datastore
data DataWrap = forall fs as. (DataSource' fs as (Apply fs as), Typeable fs, Typeable as, Typeable (Apply fs as)) => DataWrap (HList (Apply fs as))

data IOList (xs :: [*]) where
  IOCons :: IO x -> IOList xs -> IOList (x ': xs)
  IONil :: IOList '[]
  

type family (++) (l1 :: [k]) (l2 :: [k]) where
  '[]      ++ l = l
  (e ': l) ++ l' = e ': l ++ l'


type family Apply (fs :: [* -> *]) (as :: [*]) = fas | fas -> fs as where
  Apply '[] '[] = '[]
  Apply (f ': fs) (a ': as) = f a ': Apply fs as


class (xs ~ Apply fs as) => DataSource' (fs :: [* -> *]) (as :: [*]) (xs :: [*]) where
  -- | Fetch the value stored in the 'DataSource'
  -- fetch :: f a -> IO a
  fetch' :: HList xs -> IOList as
  -- | Save a value into the 'DataStore'
  --   First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  -- save :: f a -> a -> IO (f a)
  save' :: HList xs -> HList as -> IOList xs


instance {-# OVERLAPPING #-} (x ~ f a, DataSource f a) => DataSource' '[f] '[a] '[x] where
  fetch' (HCons x HNil) = IOCons (fetch x) IONil
  save' (HCons ref HNil) (HCons x HNil) = IOCons (save ref x) IONil

instance (x ~ f a, DataSource f a, DataSource' fs as xs) => DataSource' (f ': fs) (a ': as) (x ': xs) where
  fetch' (HCons x xs) = IOCons (fetch x) (fetch' xs)
  save' (HCons ref rs) (HCons x xs) = IOCons (save ref x) (save' rs xs) 









-- Pre-defined data stores

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
