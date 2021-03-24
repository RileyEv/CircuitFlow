module Pipeline.Core.HList where


data HList (xs :: [*]) where
  HCons :: x -> HList xs -> HList (x ': xs)
  HNil :: HList '[]
