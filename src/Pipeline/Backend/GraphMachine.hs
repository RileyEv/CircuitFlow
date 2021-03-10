{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Backend.GraphMachine (
  Node(..),
  processList,
  processTree,
) where

import Pipeline.Core.DataStore (DataSource(..), DataWrap(..))
import Pipeline.Core.Graph (TreeF(..))
import Pipeline.Core.IFunctor (IFix(..))
import Pipeline.Core.Task (Task(..), TaskWrap(..))
import Pipeline.Core.Node (Node(..))

import Data.Typeable (Typeable, gcast, eqT, (:~:)(..))



-- Lets start with a simple graph represented with a list. ie a -> b -> .. -> x

-- |Evaluate a single list of sequential tasks, with a starting 'DataSource'
processList :: (DataSource f a, Typeable a, Typeable f) => [Node] -> f a -> IO [Node]
processList ns firstD = do
  arr <- foldl f (return [DataNode (DataWrap firstD)]) ns
  return (tail arr)
  where
    f :: IO [Node] -> Node -> IO [Node]
    f ds' t = do
      ds <- ds'
      let dn = last ds
          nextd' = processNode dn t
      nextd <- nextd'
      return (ds ++ [nextd])

-- |Evaluate a Tree structure of tasks, with the input 'DataSource'
processTree :: (DataSource f a, Typeable f, Typeable a) => (IFix TreeF) Node -> f a -> IO ((IFix TreeF) Node)
processTree (IIn (TreeF n cs)) firstD = do
  n'@(DataNode (DataWrap d)) <- processNode (DataNode (DataWrap firstD)) n
  cs' <- mapM (`processTree` d) cs
  return (IIn (TreeF n' cs'))

-- |Evaluate a single node with the input 'DataSource' for a 'Task', returning an output 'DataSource'.
processNode :: Node -> Node -> IO Node
processNode (DataNode (DataWrap d)) (TaskNode (TaskWrap t)) = do
  r <- applyFCast d t 
  return (DataNode (DataWrap r))
processNode _ _ = error "unable to process any other combination of nodes"


applyFCast :: forall f g h a b c. (Typeable f, Typeable g, Typeable h, Typeable a, Typeable b, Typeable c) => f a -> Task g b h c -> IO (h c)
applyFCast = undefined
-- applyFCast d (Task t o) = case (eqT :: Maybe (f :~: g)) of
--   Just Refl -> applyTask d t o
--   Nothing -> error "DataSource types do not match."

applyTask :: (Typeable f, Typeable h, Typeable a, Typeable b, Typeable c) => f a -> (f b -> h c -> IO (h c)) -> h c -> IO (h c)
applyTask d t o = case gcast d of
  Just d1 -> t d1 o
  Nothing -> error "Task input and DataSource types do not match"



