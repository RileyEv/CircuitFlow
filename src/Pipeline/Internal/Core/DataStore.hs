module Pipeline.Internal.Core.DataStore (
  DataStore(..),
  DataStore'(..),
) where

import Pipeline.Internal.Common.HList (HList(..), HList'(..), IOList(..))
import Pipeline.Internal.Common.TypeList (Apply)

import Data.Kind (Type)

-- | DataStore that can be defined for each datastore needed to be used.
class DataStore f a where
  -- | Fetch the value stored in the 'DataStore'
  fetch :: f a -> IO a
  -- | Save a value into the 'DataStore'
  --
  --   The first argument depends on the instance.
  --   It may be \"empty\" or it could be a pointer to a storage location.
  save :: f a -> a -> IO (f a)

-- | When tasks require multiple inputs, they also require a joint DataStore.
--   This class provides this ability.
--
--   A user does not need to define instances of this class,
--   however it is useful when defining your own tasks.
class DataStore' (fs :: [Type -> Type]) (as :: [Type]) where
  -- | Fetch the value stored in the 'DataStore''
  fetch' :: HList' fs as -> IOList as
  -- | Save a value into the 'DataStore''
  --
  --   The first argument depends on the instance.
  --   It may be \"empty\" or it could be a pointer to a storage location.
  save' :: HList' fs as -> HList as -> IOList (Apply fs as)


instance {-# OVERLAPPING #-} (DataStore f a) => DataStore' '[f] '[a] where
  fetch' (HCons' x HNil') = IOCons (fetch x) IONil
  save' (HCons' ref HNil') (HCons x HNil) = IOCons (save ref x) IONil

instance (DataStore f a, DataStore' fs as) => DataStore' (f ': fs) (a ': as)  where
  fetch' (HCons' x xs) = IOCons (fetch x) (fetch' xs)
  save' (HCons' ref rs) (HCons x xs) = IOCons (save ref x) (save' rs xs) 









