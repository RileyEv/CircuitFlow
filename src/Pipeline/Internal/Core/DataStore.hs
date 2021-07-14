module Pipeline.Internal.Core.DataStore
  ( DataStore(..)
  , DataStore'(..)
  ) where

import           Data.Kind                         (Type)
import           Pipeline.Internal.Common.HList    (HList (..), HList' (..))
import           Pipeline.Internal.Core.UUID       (UUID)

-- | DataStore that can be defined for each datastore needed to be used.
class DataStore f a where
  -- | Fetch the value stored in the 'DataStore'
  fetch :: UUID -> f a -> IO a
  -- | Save a value into the 'DataStore'
  --
  --   The first argument depends on the instance.
  --   It may be \"empty\" or it could be a pointer to a storage location.
  save :: UUID -> f a -> a -> IO ()

-- | When tasks require multiple inputs, they also require a joint DataStore.
--   This class provides this ability.
--
--   A user does not need to define instances of this class,
--   however it is useful when defining your own tasks.
class DataStore' (fs :: [Type -> Type]) (as :: [Type]) where
  -- | Fetch the value stored in the 'DataStore''
  fetch' :: UUID -> HList' fs as -> IO (HList as)
  -- | Save a value into the 'DataStore''
  --
  --   The first argument depends on the instance.
  --   It may be \"empty\" or it could be a pointer to a storage location.
  save' :: UUID -> HList' fs as -> HList as -> IO ()


instance {-# OVERLAPPING #-} (DataStore f a) => DataStore' '[f] '[a] where
  fetch' uuid (HCons' x HNil') = fetch uuid x >>= \y -> return (HCons y HNil)
  save' uuid (HCons' ref HNil') (HCons x HNil) = save uuid ref x

instance {-# OVERLAPPABLE #-} (DataStore f a, DataStore' fs as) => DataStore' (f ': fs) (a ': as)  where

  fetch' uuid (HCons' x xs) = (return $ HCons) <*> fetch uuid x <*> fetch' uuid xs
  save' uuid (HCons' ref rs) (HCons x xs) = (save uuid ref x) >> (save' uuid rs xs)


-- {-|
--   A 'VariableStore' is a simple in memory 'DataStore'.
-- -}
-- data VariableStore a = Var a | Empty deriving (Eq, Show, Generic, NFData)


-- instance DataStore VariableStore a where
--   fetch _ (Var x) = return x
--   fetch _ Empty   = error "empty source"
--   save _ _ _ = return ()
