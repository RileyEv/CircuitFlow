{-# LANGUAGE PolyKinds #-}
module Pipeline.Internal.Common.IFunctor where


class IFunctor iF where
  imap :: (forall a. f a -> g a) -> iF f a -> iF g a

class IFunctor2 iF where
  imap2 :: (forall a b. f a b -> g a b) -> iF f a b -> iF g a b
  
class IFunctor4 iF where
  imap4 :: (forall a b c d. f a b c d -> g a b c d) -> iF f a b c d -> iF g a b c d

class IFunctor6 iF where
  imap6 :: (forall a b c d e f. f' a b c d e f -> g' a b c d e f) -> iF f' a b c d e f -> iF g' a b c d e f
  
class IFunctor7 iF where
  imap7 :: (forall a b c d e f g. f' a b c d e f g -> g' a b c d e f g) -> iF f' a b c d e f g -> iF g' a b c d e f g
  
newtype Fix f = In (f (Fix f))
newtype IFix  iF a = IIn  (iF (IFix  iF) a)
newtype IFix2 iF a b = IIn2 (iF (IFix2 iF) a b)
newtype IFix3 iF a b c = IIn3 (iF (IFix3 iF) a b c)
newtype IFix4 iF a b c d = IIn4 (iF (IFix4 iF) a b c d)
newtype IFix5 iF a b c d e = IIn5 (iF (IFix5 iF) a b c d e)
newtype IFix6 iF a b c d e f = IIn6 (iF (IFix6 iF) a b c d e f)
newtype IFix7 iF a b c d e f g = IIn7 (iF (IFix7 iF) a b c d e f g)

icata :: IFunctor iF => (forall a. iF f a -> f a) -> IFix iF a -> f a
icata alg (IIn x) = alg (imap (icata alg) x)

icata2 :: IFunctor2 iF => (forall a b. iF f a b -> f a b) -> IFix2 iF a b -> f a b
icata2 alg (IIn2 x) = alg (imap2 (icata2 alg) x)

icata4 :: IFunctor4 iF => (forall a b c d. iF f a b c d -> f a b c d) -> IFix4 iF a b c d -> f a b c d
icata4 alg (IIn4 x) = alg (imap4 (icata4 alg) x)

newtype C4 a i j k l = C4 {unConst4 :: a}

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In x) = alg (fmap (cata alg) x)


