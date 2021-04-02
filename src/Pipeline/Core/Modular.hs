{-# Language PolyKinds, MultiParamTypeClasses #-}
module Pipeline.Core.Modular where

import Pipeline.Core.IFunctor (IFunctor6(..))

data (iF :+: iG) (f' :: k -> j -> i -> k -> j -> i -> *) (a :: k) (b :: j) (c :: i) (d :: k) (e :: j) (f :: i) where
  L :: iF f' a b c d e f -> (iF :+: iG) f' a b c d e f
  R :: iG f' a b c d e f -> (iF :+: iG) f' a b c d e f

infixr :+:

instance (IFunctor6 iF, IFunctor6 iG) => IFunctor6 (iF :+: iG) where
  imap6 f (L x) = L (imap6 f x)
  imap6 f (R y) = R (imap6 f y)
  

class (IFunctor6 iF, IFunctor6 iG) => iF :<: iG where
  inj :: iF f' a b c d e f -> iG f' a b c d e f

instance IFunctor6 iF => iF :<: iF where
  inj = id

instance {-# OVERLAPPING #-} (IFunctor6 iF, IFunctor6 iG) => iF :<: (iF :+: iG) where
  inj = L

instance (IFunctor6 iF, IFunctor6 iG, IFunctor6 iH, iF :<: iG) => iF :<: (iH :+: iG) where
  inj = R . inj
