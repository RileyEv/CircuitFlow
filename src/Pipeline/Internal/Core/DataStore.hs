module Pipeline.Internal.Core.DataStore (
  DataStore(..),
  DataStore'(..),
) where

import Pipeline.Internal.Common.HList (HList(..), HList'(..), IOList(..))
import Pipeline.Internal.Common.TypeList (Apply)
import Pipeline.Internal.Core.UUID (UUID)

import Data.Kind (Type)

-- | DataStore that can be defined for each datastore needed to be used.
class DataStore f a where
  -- | Fetch the value stored in the 'DataStore'
  fetch :: UUID -> f a -> IO a
  -- | Save a value into the 'DataStore'
  --
  --   The first argument depends on the instance.
  --   It may be \"empty\" or it could be a pointer to a storage location.
  save :: UUID -> f a -> a -> IO (f a)

-- | When tasks require multiple inputs, they also require a joint DataStore.
--   This class provides this ability.
--
--   A user does not need to define instances of this class,
--   however it is useful when defining your own tasks.
class DataStore' (fs :: [Type -> Type]) (as :: [Type]) where
  -- | Fetch the value stored in the 'DataStore''
  fetch' :: UUID -> HList' fs as -> IOList as
  -- | Save a value into the 'DataStore''
  --
  --   The first argument depends on the instance.
  --   It may be \"empty\" or it could be a pointer to a storage location.
  save' :: UUID -> HList' fs as -> HList as -> IOList (Apply fs as)


instance {-# OVERLAPPING #-} (DataStore f a) => DataStore' '[f] '[a] where
  fetch' uuid (HCons' x HNil') = IOCons (fetch uuid x) IONil
  save' uuid (HCons' ref HNil') (HCons x HNil) = IOCons (save uuid ref x) IONil

instance (DataStore f a, DataStore' fs as) => DataStore' (f ': fs) (a ': as)  where
  fetch' uuid (HCons' x xs) = IOCons (fetch uuid x) (fetch' uuid xs)
  save' uuid (HCons' ref rs) (HCons x xs) = IOCons (save uuid ref x) (save' uuid rs xs) 









