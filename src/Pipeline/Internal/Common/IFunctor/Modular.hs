{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
module Pipeline.Internal.Common.IFunctor.Modular where

import           Pipeline.Internal.Common.IFunctor (IFunctor5 (..))

import           Data.Kind                         (Type)

data (iF :+: iG) (f' :: i -> j -> i -> j -> k -> Type) (a :: i) (b :: j) (c :: i) (d :: j) (e :: k) where
  L ::iF f' a b c d e -> (iF :+: iG) f' a b c d e
  R ::iG f' a b c d e -> (iF :+: iG) f' a b c d e

infixr :+:

instance (IFunctor5 iF, IFunctor5 iG) => IFunctor5 (iF :+: iG) where
  imap5 f (L x) = L (imap5 f x)
  imap5 f (R y) = R (imap5 f y)
  imapM5 f (L x) = do
    x' <- imapM5 f x
    return (L x')
  imapM5 f (R y) = do
    y' <- imapM5 f y
    return (R y')


class (IFunctor5 iF, IFunctor5 iG) => iF :<: iG where
  inj :: iF f' a b c d e -> iG f' a b c d e

instance IFunctor5 iF => iF :<: iF where
  inj = id

instance {-# OVERLAPPING #-} (IFunctor5 iF, IFunctor5 iG) => iF :<: (iF :+: iG) where
  inj = L

instance (IFunctor5 iF, IFunctor5 iG, IFunctor5 iH, iF :<: iG) => iF :<: (iH :+: iG) where
  inj = R . inj
