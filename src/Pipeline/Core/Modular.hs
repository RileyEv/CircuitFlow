{-# Language PolyKinds, MultiParamTypeClasses #-}
module Pipeline.Core.Modular where

import Pipeline.Core.IFunctor (IFunctor2(..))

data (iF :+: iG) (f :: k -> k -> *) (a :: k) (b :: k) where
  L :: iF f a b -> (iF :+: iG) f a b
  R :: iG f a b -> (iF :+: iG) f a b

infixr :+:

instance (IFunctor2 iF, IFunctor2 iG) => IFunctor2 (iF :+: iG) where
  imap2 f (L x) = L (imap2 f x)
  imap2 f (R y) = R (imap2 f y)
  

class (IFunctor2 iF, IFunctor2 iG) => iF :<: iG where
  inj :: iF f a b -> iG f a b

instance IFunctor2 iF => iF :<: iF where
  inj = id

instance {-# OVERLAPPING #-} (IFunctor2 iF, IFunctor2 iG) => iF :<: (iF :+: iG) where
  inj = L

instance (IFunctor2 iF, IFunctor2 iG, IFunctor2 iH, iF :<: iG) => iF :<: (iH :+: iG) where
  inj = R . inj
