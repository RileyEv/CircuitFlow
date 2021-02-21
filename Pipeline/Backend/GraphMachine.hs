{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Backend.GraphMachine (
  Node(..),
  TreeF(..),
  TreeAlg,
  processList,
  processTree,
) where

import Pipeline.Core.Task (Task(..))
import Pipeline.Core.DataStore (DataSource(..))
import Pipeline.Core.IFunctor (IFix(..), IFunctor(..))

import Data.Typeable (Typeable, gcast, eqT, (:~:)(..))

-- data Tree a = Tree a [Tree a]
data TreeF f a = TreeF a [f a]

instance IFunctor TreeF where
  imap f (TreeF x ts) = TreeF x (map f ts)

type TreeAlg f a = TreeF f a -> f a

instance Functor (IFix TreeF) where
  fmap f (IIn (TreeF x ts)) = IIn (TreeF (f x) (map (fmap f) ts))




data Node = forall f a g b. (Typeable f, Typeable g, Typeable a, Typeable b, DataSource f a, DataSource g b) => TaskNode (Task f a g b)
          | forall f a. (Typeable f, Typeable a, DataSource f a) => DataNode (f a)


-- Lets start with a simple graph represented with a list. ie a -> b -> .. -> x
processList :: (DataSource f a, Typeable a, Typeable f) => [Node] -> f a -> IO [Node]
processList ns firstD = do
  arr <- foldl f (return [DataNode firstD]) ns
  return (tail arr)
  where
    f :: IO [Node] -> Node -> IO [Node]
    f ds' t = do
      ds <- ds'
      let dn = last ds
          nextd' = processNode dn t
      nextd <- nextd'
      return (ds ++ [nextd])


processTree :: (DataSource f a, Typeable f, Typeable a) => (IFix TreeF) Node -> f a -> IO ((IFix TreeF) Node)
processTree (IIn (TreeF n cs)) firstD = do
  n'@(DataNode d) <- processNode (DataNode firstD) n
  cs' <- mapM (`processTree` d) cs
  return (IIn (TreeF n' cs'))

processNode :: Node -> Node -> IO Node
processNode (DataNode d) (TaskNode t) = do
  r <- applyFCast d t 
  return (DataNode r)
processNode _ _ = error "unable to process any other combination of nodes"


applyFCast :: forall f g h a b c. (Typeable f, Typeable g, Typeable h, Typeable a, Typeable b, Typeable c) => f a -> Task g b h c -> IO (h c)
applyFCast d (Task t o) = case (eqT :: Maybe (f :~: g)) of
  Just Refl -> applyTask d t o
  Nothing -> error "DataSource types do not match."
  


applyTask :: (Typeable f, Typeable h, Typeable a, Typeable b, Typeable c) => f a -> (f b -> h c -> IO (h c)) -> h c -> IO (h c)
applyTask d t o = case gcast d of
  Just d1 -> t d1 o
  Nothing -> error "Task input and DataSource types do not match"



