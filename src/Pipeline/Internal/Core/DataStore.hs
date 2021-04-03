{-# LANGUAGE MultiParamTypeClasses, DataKinds, PolyKinds, TypeFamilyDependencies, ExplicitNamespaces #-}

module Pipeline.Internal.Core.DataStore (
  DataStore(..),
  DataStore'(..),
) where

import Pipeline.Internal.Common.HList (HList(..), HList'(..), IOList(..))
import Pipeline.Internal.Common.TypeList (Apply)

import Data.Kind (Type)

-- DataSource that can be defined for each datastore to be used.
class DataStore f a where
  -- | Fetch the value stored in the 'DataSource'
  fetch :: f a -> IO a
  -- | Save a value into the 'DataStore'
  --   First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  save :: f a -> a -> IO (f a)


class DataStore' (fs :: [Type -> Type]) (as :: [Type]) where
  -- | Fetch the value stored in the 'DataSource'
  -- fetch :: f a -> IO a
  fetch' :: HList' fs as -> IOList as
  -- | Save a value into the 'DataStore'
  --   First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  -- save :: f a -> a -> IO (f a)
  save' :: HList' fs as -> HList as -> IOList (Apply fs as)


instance {-# OVERLAPPING #-} (DataStore f a) => DataStore' '[f] '[a] where
  fetch' (HCons' x HNil') = IOCons (fetch x) IONil
  save' (HCons' ref HNil') (HCons x HNil) = IOCons (save ref x) IONil

instance (DataStore f a, DataStore' fs as) => DataStore' (f ': fs) (a ': as)  where
  fetch' (HCons' x xs) = IOCons (fetch x) (fetch' xs)
  save' (HCons' ref rs) (HCons x xs) = IOCons (save ref x) (save' rs xs) 









