{-# LANGUAGE PolyKinds #-}
module Pipeline.Core.IFunctor where


class IFunctor iF where
  imap :: (forall a. f a -> g a) -> iF f a -> iF g a

class IFunctor4 iF where
  imap4 :: (forall a b c d. f a b c d -> g a b c d) -> iF f a b c d -> iF g a b c d

newtype Fix f = In (f (Fix f))
newtype IFix iF a = IIn (iF (IFix iF) a)
newtype IFix4 iF a b c d = IIn4 (iF (IFix4 iF) a b c d)

icata :: IFunctor iF => (forall a. iF f a -> f a) -> IFix iF a -> f a
icata alg (IIn x) = alg (imap (icata alg) x)

icata4 :: IFunctor4 iF => (forall a b c d. iF f a b c d -> f a b c d) -> IFix4 iF a b c d -> f a b c d
icata4 alg (IIn4 x) = alg (imap4 (icata4 alg) x)

newtype C4 a i j k l = C4 {unConst4 :: a}

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In x) = alg (fmap (cata alg) x)

