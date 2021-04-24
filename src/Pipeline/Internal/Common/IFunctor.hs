{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pipeline.Internal.Common.IFunctor where

import           Data.Kind                    (Type)
import           Pipeline.Internal.Common.Nat (Nat)

class IFunctor iF where
  imap :: (forall a. f a -> g a) -> iF f a -> iF g a
  imapM :: Monad m => (forall a. f a -> m (g a)) -> iF f a -> m (iF g a)

class IFunctor2 iF where
  imap2 :: (forall a b. f a b -> g a b) -> iF f a b -> iF g a b

class IFunctor4 iF where
  imap4 :: (forall a b c d. f a b c d -> g a b c d) -> iF f a b c d -> iF g a b c d

class IFunctor6 iF where
  imap6 :: (forall a b c d e f. f' a b c d e f -> g' a b c d e f) -> iF f' a b c d e f -> iF g' a b c d e f

class IFunctor7 (iF :: (aK -> bK -> cK -> dK -> eK -> fK -> Nat -> Type) -> aK -> bK -> cK -> dK -> eK -> fK -> Nat -> Type) where
  imap7 :: (forall a b c d e f g. f' a b c d e f g -> g' a b c d e f g) -> iF f' a b c d e f g -> iF g' a b c d e f g
  imapM7 :: forall f' g' (a :: aK) (b :: bK) (c :: cK) (d :: dK) (e :: eK) (f :: fK) (g :: Nat) m . Monad m
    => (forall (a :: aK) (b :: bK) (c :: cK) (d :: dK) (e :: eK) (f :: fK) (g :: Nat). f' a b c d e f (g :: Nat) -> m (g' a b c d e f (g :: Nat)))
    -> iF f' a b c d e f (g :: Nat)
    -> m (iF g' a b c d e f (g :: Nat))

newtype Fix f = In (f (Fix f))
newtype IFix  iF a = IIn  (iF (IFix  iF) a)
newtype IFix2 iF a b = IIn2 (iF (IFix2 iF) a b)
newtype IFix3 iF a b c = IIn3 (iF (IFix3 iF) a b c)
newtype IFix4 iF a b c d = IIn4 (iF (IFix4 iF) a b c d)
newtype IFix5 iF a b c d e = IIn5 (iF (IFix5 iF) a b c d e)
newtype IFix6 iF a b c d e f = IIn6 (iF (IFix6 iF) a b c d e f)
newtype IFix7 iF a b c d e f (g :: Nat) = IIn7 (iF (IFix7 iF) a b c d e f g)

icata :: IFunctor iF => (forall a . iF f a -> f a) -> IFix iF a -> f a
icata alg (IIn x) = alg (imap (icata alg) x)

icata2 :: IFunctor2 iF => (forall a b . iF f a b -> f a b) -> IFix2 iF a b -> f a b
icata2 alg (IIn2 x) = alg (imap2 (icata2 alg) x)

icata4
  :: IFunctor4 iF => (forall a b c d . iF f a b c d -> f a b c d) -> IFix4 iF a b c d -> f a b c d
icata4 alg (IIn4 x) = alg (imap4 (icata4 alg) x)

icata7
  :: IFunctor7 iF
  => (forall a b c d e f g . iF f' a b c d e f g -> f' a b c d e f g)
  -> IFix7 iF a b c d e f g
  -> f' a b c d e f g
icata7 alg (IIn7 x) = alg (imap7 (icata7 alg) x)

icataM7
  :: (IFunctor7 iF, Monad m)
  => (forall a b c d e f (g :: Nat) . iF f' a b c d e f g -> m (f' a b c d e f g))
  -> IFix7 iF a b c d e f (g :: Nat)
  -> m (f' a b c d e f (g :: Nat))
icataM7 algM (IIn7 x) = algM =<< imapM7 (icataM7 algM) x


-- https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.3718&rep=rep1&type=pdf
-- https://research-information.bris.ac.uk/ws/files/65842535/Nicolas_Wu_Unifying_Structured_Recursion_Schemes.pdf
-- http://www.timphilipwilliams.com/slides/HaskellAtBarclays.pdf
-- https://jtobin.io/monadic-recursion-schemes
icataM :: (IFunctor iF, Monad m) => (forall a . iF f a -> m (f a)) -> IFix iF a -> m (f a)
icataM algM (IIn x) = algM =<< imapM (icataM algM) x
