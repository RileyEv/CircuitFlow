module Pipeline.Core.Graph (
  TreeF(..),
  TaskTreeF(..),
  DataTreeF(..),
  FList(..),
  TreeAlg
) where

import Pipeline.Core.IFunctor (IFunctor(..), IFix(..))
import Pipeline.Core.Task (Task)
import Pipeline.Core.DataStore (Apply, DataSource', HAppendListR, HList)

-- |Structure used to represent a pipeline
data TreeF f a = TreeF a [f a] deriving Show

data TaskTreeF f i o where
  TBranchF :: Task fs as g b
          -> FList f '[g b] hcs
          -> TaskTreeF f (Apply fs as) hcs
  TLeafF :: (is ~ Apply fs as, DataSource' fs as is, DataSource' '[g] '[b] '[g b]) => Task fs as g b -> TaskTreeF f is (Apply '[g] '[b])


data DataTreeF f i o where
  DBranchF :: (DataSource' fs as (Apply fs as)) => HList (Apply fs as) -> FList f (Apply fs as) gbs -> DataTreeF f (Apply fs as) gbs
  DLeafF :: (fas ~ Apply fs as, DataSource' fs as fas) => HList fas -> DataTreeF f fas fas

data FList (f :: [*] -> [*] -> *) (is :: [*]) (xs :: [*]) where
  FCons :: f is xs -> FList f is ys -> FList f is (HAppendListR xs ys)
  FNil  :: FList f is '[]


instance IFunctor TreeF where
  imap f (TreeF x ts) = TreeF x (map f ts)

type TreeAlg f a = TreeF f a -> f a

instance Functor (IFix TreeF) where
  fmap f (IIn (TreeF x ts)) = IIn (TreeF (f x) (map (fmap f) ts))
