module Pipeline.Internal.Common.HList where

import           Data.Kind (Type)

-- | A heterogeneous list used as input/output to a task.
--
--   This is commonly used in the 'Pipeline.Task.multiInputTask' function,
--   which automatically fetches the data from 'Pipeline.DataStore.DataStore'.
data HList (xs :: [Type]) where
  HCons :: x -> HList xs -> HList (x ': xs)
  HNil :: HList '[]

-- | A heterogeneous list used as input/output to a network or task.
data HList' (fs :: [Type -> Type]) (as :: [Type]) where
  HCons' :: (Eq (f a), Show (f a)) => f a -> HList' fs as -> HList' (f ': fs) (a ': as)
  HNil' :: HList' '[] '[]


instance Show (HList' fs as) where
  show HNil'         = "HNil'"
  show (HCons' x xs) = concat ["HCons' (", show x, ") (", show xs, ")"]

instance Eq (HList' fs as) where
  HNil'         == HNil'         = True
  (HCons' x xs) == (HCons' y ys) = x == y && xs == ys


