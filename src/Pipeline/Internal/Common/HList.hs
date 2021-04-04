module Pipeline.Internal.Common.HList where


import Data.Kind (Type)

data HList (xs :: [Type]) where
  HCons :: x -> HList xs -> HList (x ': xs)
  HNil :: HList '[]

data HList' (fs :: [Type -> Type]) (as :: [Type]) where
  HCons' :: (Eq (f a), Show (f a)) => f a -> HList' fs as -> HList' (f ': fs) (a ': as)
  HNil' :: HList' '[] '[]

data IOList (xs :: [Type]) where
  IOCons :: IO x -> IOList xs -> IOList (x ': xs)
  IONil :: IOList '[]


instance Show (HList' fs as) where
  show HNil' = "HNil'"
  show (HCons' x xs) = concat ["HCons' (", show x, ") (", show xs ,")"]
  
instance Eq (HList' fs as) where
  HNil'         == HNil'         = True
  (HCons' x xs) == (HCons' y ys) = x == y && xs == ys
