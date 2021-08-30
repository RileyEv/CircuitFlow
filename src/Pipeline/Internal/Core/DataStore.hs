module Pipeline.Internal.Core.DataStore
  ( DataStore(..)
  , DataStore'(..)
  , Var
  , emptyVar
  ) where

import           Data.Kind                         (Type)
import           Pipeline.Internal.Common.HList    (HList (..), HList' (..))
import           Pipeline.Internal.Core.UUID       (JobUUID, TaskUUID)
import           Control.Concurrent.MVar           (MVar, putMVar, readMVar, newEmptyMVar)


-- | DataStore that can be defined for each datastore needed to be used.
class DataStore f a where
  -- | Fetch the value stored in the 'DataStore'
  fetch :: f a -> IO a
  -- | Save a value into the 'DataStore'
  --
  --   The first argument depends on the instance.
  --   It may be \"empty\" or it could be a pointer to a storage location.
  save :: f a -> a -> IO ()
  -- | Returns a new empty store
  empty :: TaskUUID -> JobUUID -> IO (f a)


-- | When tasks require multiple inputs, they also require a joint DataStore.
--   This class provides this ability.
--
--   A user does not need to define instances of this class,
--   however it is useful when defining your own tasks.
class DataStore' (fs :: [Type -> Type]) (as :: [Type]) where
  -- | Fetch the value stored in the 'DataStore''
  fetch' :: HList' fs as -> IO (HList as)
  -- | Save a value into the 'DataStore''
  --
  --   The first argument depends on the instance.
  --   It may be \"empty\" or it could be a pointer to a storage location.
  save' :: HList' fs as -> HList as -> IO ()
  empty' :: TaskUUID -> JobUUID -> IO (HList' fs as)

instance {-# OVERLAPPING #-} (DataStore f a, Eq a, Eq (f a)) => DataStore' '[f] '[a] where
  fetch' (HCons' x HNil') = fetch x >>= \y -> return (HCons y HNil)
  save' (HCons' ref HNil') (HCons x HNil) = save ref x
  empty' taskUUID jobUUID = HCons' <$> empty taskUUID jobUUID <*> return HNil'

instance {-# OVERLAPPABLE #-} (DataStore f a, DataStore' fs as, Eq a, Eq (f a)) => DataStore' (f ': fs) (a ': as)  where
  fetch' (HCons' x xs) = HCons <$> fetch x <*> fetch' xs
  save' (HCons' ref rs) (HCons x xs) = save ref x >> save' rs xs
  empty' taskUUID jobUUID = HCons' <$> empty taskUUID jobUUID <*> empty' taskUUID jobUUID


-- | Simple in memory variable store, and unmutable.
newtype Var a = Var {unVar :: MVar a} deriving (Eq)

emptyVar :: IO (Var a)
emptyVar = Var <$> newEmptyMVar

instance DataStore Var a where
  fetch = readMVar . unVar
  save = putMVar . unVar
  empty _ _ = emptyVar
