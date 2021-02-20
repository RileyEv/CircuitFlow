{-# LANGUAGE PolyKinds #-}
module Pipeline.Core.IFunctor where


class IFunctor iF where
  imap :: (forall a. f a -> g a) -> iF f a -> iF g a

class IFunctor4 iF where
  imap4 :: (forall a b c d. f a b c d -> g a b c d) -> iF f a b c d -> iF g a b c d

newtype Fix iF a = In (iF (Fix iF) a)
newtype Fix4 iF a b c d = In4 (iF (Fix4 iF) a b c d)

cata :: IFunctor iF => (forall a. iF f a -> f a) -> Fix iF a -> f a
cata alg (In x) = alg (imap (cata alg) x)


cata4 :: IFunctor4 iF => (forall a b c d. iF f a b c d -> f a b c d) -> Fix4 iF a b c d -> f a b c d
cata4 alg (In4 x) = alg (imap4 (cata4 alg) x)

newtype C4 a i j k l = C4 {unConst4 :: a}
