module Pipeline.Internal.Common.HList where


import Data.Kind (Type)

data HList (xs :: [Type]) where
  HCons :: x -> HList xs -> HList (x ': xs)
  HNil :: HList '[]

data HList' (fs :: [Type -> Type]) (as :: [Type]) where
  HCons' :: f a -> HList' fs as -> HList' (f ': fs) (a ': as)
  HNil' :: HList' '[] '[]

data IOList (xs :: [Type]) where
  IOCons :: IO x -> IOList xs -> IOList (x ': xs)
  IONil :: IOList '[]
