{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Backend.GraphMachine (
  Node(..),
  -- processList,
  processTree,
) where

import Pipeline.Core.DataStore (DataSource'(..), DataWrap(..), HList(..), Apply)
import Pipeline.Core.Graph (TreeF(..))
import Pipeline.Core.IFunctor (IFix(..))
import Pipeline.Core.Task (Task(..), TaskWrap(..))
import Pipeline.Core.Node (Node(..))

import Data.Typeable (Typeable, gcast, eqT, (:~:)(..))



-- |Evaluate a Tree structure of tasks, with the input 'DataSource'
processTree :: (DataSource' fs as (Apply fs as), Typeable fs, Typeable as, Typeable (Apply fs as)) => (IFix TreeF) Node -> HList (Apply fs as) -> IO ((IFix TreeF) Node)
processTree (IIn (TreeF n cs)) firstD = do
  n'@(DataNode (DataWrap d)) <- processNode (DataNode (DataWrap firstD)) n
  cs' <- mapM (`processTree` d) cs
  return (IIn (TreeF n' cs'))

-- |Evaluate a single node with the input 'DataSource' for a 'Task', returning an output 'DataSource'.
processNode :: Node -> Node -> IO Node
processNode (DataNode (DataWrap d)) (TaskNode (TaskWrap t)) = do
  r <- applyFCast d t 
  return (DataNode (DataWrap (HCons r HNil)))
processNode _ _ = error "unable to process any other combination of nodes"



applyFCast :: forall fs gs h as bs c. (
  Typeable fs, Typeable gs, Typeable h, Typeable as, Typeable bs, Typeable c,
  Typeable (Apply fs as), Typeable (Apply gs bs)) => HList (Apply fs as) -> Task gs bs h c -> IO (h c)
applyFCast d (Task t o) = case (eqT :: Maybe (fs :~: gs)) of
  Just Refl -> applyTask d t o
  Nothing -> error "DataSource types do not match."

applyTask :: (Typeable fs, Typeable h, Typeable as, Typeable bs, Typeable c, Typeable (Apply fs as), Typeable (Apply fs bs)) => HList (Apply fs as) -> (HList (Apply fs bs) -> h c -> IO (h c)) -> h c -> IO (h c)
applyTask d t o = case gcast d of
  Just d1 -> t d1 o
  Nothing -> error "Task input and DataSource types do not match"



