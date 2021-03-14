{-# Language PolyKinds #-}
module Pipeline.Core.Modular where

import Pipeline.Core.IFunctor (IFunctor2(..))

data (iF :+: iG) (f :: k -> k -> *) (a :: k) (b :: k) where
  L :: iF f a b -> (iF :+: iG) f a b
  R :: iG f a b -> (iF :+: iG) f a b

infixr :+:

instance (IFunctor2 iF, IFunctor2 iG) => IFunctor2 (iF :+: iG) where
  imap2 f (L x) = L (imap2 f x)
  imap2 f (R y) = R (imap2 f y)
  
