module Pipeline.Core.Graph (
  TreeF(..),
  TreeAlg
) where

import Pipeline.Core.IFunctor (IFunctor(..), IFix(..))

-- |Structure used to represent a pipeline
data TreeF f a = TreeF a [f a] deriving Show

instance IFunctor TreeF where
  imap f (TreeF x ts) = TreeF x (map f ts)

type TreeAlg f a = TreeF f a -> f a

instance Functor (IFix TreeF) where
  fmap f (IIn (TreeF x ts)) = IIn (TreeF (f x) (map (fmap f) ts))
