{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
module Pipeline.Internal.Common.IFunctor.Modular where

import           Pipeline.Internal.Common.IFunctor (IFunctor7 (..))
import           Pipeline.Internal.Common.Nat      (Nat)

import           Data.Kind                         (Type)

data (iF :+: iG) (f' :: k -> j -> i -> k -> j -> i -> Nat -> Type) (a :: k) (b :: j) (c :: i) (d :: k) (e :: j) (f :: i) (g :: Nat) where
  L ::iF f' a b c d e f g -> (iF :+: iG) f' a b c d e f g
  R ::iG f' a b c d e f g -> (iF :+: iG) f' a b c d e f g

infixr :+:

instance (IFunctor7 iF, IFunctor7 iG) => IFunctor7 (iF :+: iG) where
  imap7 f (L x) = L (imap7 f x)
  imap7 f (R y) = R (imap7 f y)


class (IFunctor7 iF, IFunctor7 iG) => iF :<: iG where
  inj :: iF f' a b c d e f g -> iG f' a b c d e f g

instance IFunctor7 iF => iF :<: iF where
  inj = id

instance {-# OVERLAPPING #-} (IFunctor7 iF, IFunctor7 iG) => iF :<: (iF :+: iG) where
  inj = L

instance (IFunctor7 iF, IFunctor7 iG, IFunctor7 iH, iF :<: iG) => iF :<: (iH :+: iG) where
  inj = R . inj
