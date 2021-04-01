{-# Language PolyKinds, MultiParamTypeClasses #-}
module Pipeline.Core.Modular where

import Pipeline.Core.IFunctor (IFunctor4(..))

data (iF :+: iG) (f :: k -> j -> k -> j -> *) (a :: k) (b :: j) (c :: k) (d :: j)where
  L :: iF f a b c d -> (iF :+: iG) f a b c d
  R :: iG f a b c d -> (iF :+: iG) f a b c d

infixr :+:

instance (IFunctor4 iF, IFunctor4 iG) => IFunctor4 (iF :+: iG) where
  imap4 f (L x) = L (imap4 f x)
  imap4 f (R y) = R (imap4 f y)
  

class (IFunctor4 iF, IFunctor4 iG) => iF :<: iG where
  inj :: iF f a b c d -> iG f a b c d

instance IFunctor4 iF => iF :<: iF where
  inj = id

instance {-# OVERLAPPING #-} (IFunctor4 iF, IFunctor4 iG) => iF :<: (iF :+: iG) where
  inj = L

instance (IFunctor4 iF, IFunctor4 iG, IFunctor4 iH, iF :<: iG) => iF :<: (iH :+: iG) where
  inj = R . inj
