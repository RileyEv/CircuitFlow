module Pipeline.Core.HList where


data HList (xs :: [*]) where
  HCons :: x -> HList xs -> HList (x ': xs)
  HNil :: HList '[]


data HList' (fs :: [* -> *]) (as :: [*]) where
  HCons' :: f a -> HList' fs as -> HList' (f ': fs) (a ': as)
  HNil' :: HList' '[] '[]
