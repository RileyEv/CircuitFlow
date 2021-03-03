module Pipeline.Frontend.Workflow where

import Pipeline.Core.DataStore (DataSource)
import Pipeline.Core.IFunctor (IFix(..), C4(..), IFix4(..), icata4)
import Pipeline.Core.Task (Task, TaskWrap(..))
import Pipeline.Core.Graph (TreeF(..))
import Pipeline.Core.Node (Node(..))
import Pipeline.Frontend.PID (PID)
import Pipeline.Frontend.Pipe (Chain, ChainF(..), Pipe(..), ChainAlg)

import Control.Monad.State (State, get, put, runState)

import qualified Data.Map as M (Map, insert, empty, lookup)
import Data.Typeable (Typeable)


-- |Stores the state required to keep track of registered tasks and their PIDs.
data WorkflowState = WorkflowState {
  pidCounter :: PID,
  tasks :: M.Map PID TaskWrap
}


-- |A monad that is used when defining a pipeline.
type Workflow = State WorkflowState


-- |Used to add a task to the State
insertTask :: (DataSource f a, DataSource g b, Typeable f, Typeable a, Typeable g, Typeable b) => PID -> Task f a g b -> Workflow ()
insertTask pid t = do
  s <- get
  let s' = WorkflowState (pidCounter s) (M.insert pid (TaskWrap t) (tasks s))
  put s'
  
-- |Retrieves the next valid PID when registering tasks.
nextPID :: Workflow PID
nextPID = do
  s <- get
  let newPID = pidCounter s + 1
  put (WorkflowState newPID (tasks s))
  return newPID

-- |Registers a task in the pipeline. This gets a new pid and adds the task to the state.
registerTask :: (DataSource f a, DataSource g b, Typeable f, Typeable a, Typeable g, Typeable b) => Task f a g b -> Workflow (Chain f a g b)
registerTask t = do
  pid <- nextPID
  insertTask pid t
  return $ IIn4 (ProcessF pid)

-- |Convert a Pipe into a tree structure for the backend.
pipeToTree :: Pipe -> (IFix TreeF) PID
-- Fold chain into a linear Tree of PIDs
pipeToTree (Pipe c) = unConst4 (icata4 alg c)
  where
    alg :: ChainAlg (C4 ((IFix TreeF) PID)) a b c d
    alg (JoinF (C4 (IIn (TreeF n _))) (C4 y)) = C4 (IIn (TreeF n [y]))
    alg (ProcessF pid) = C4 (IIn (TreeF pid []))
pipeToTree (And l r) = let l' = pipeToTree l
                           r' = pipeToTree r
                       in insertIntoTree r' l'

-- |Inserts a new chain into the tree.
insertIntoTree :: (IFix TreeF) PID -> (IFix TreeF) PID -> (IFix TreeF) PID
insertIntoTree t@(IIn (TreeF n' cs')) (IIn (TreeF n cs))
  | n == n'   = IIn (TreeF n (cs' ++ cs))
  | otherwise = IIn (TreeF n (map (insertIntoTree t) cs))

-- |Inject all the functions into the nodes of the tree.
pidTreeToNodeTree :: M.Map PID TaskWrap -> (IFix TreeF) PID -> (IFix TreeF) Node
pidTreeToNodeTree m = fmap (\x -> case M.lookup x m of
      Just (TaskWrap n) -> TaskNode (TaskWrap n)
      Nothing -> error "Not in the map")


emptyWorkflowState :: WorkflowState
emptyWorkflowState = WorkflowState (-1) M.empty

buildTree :: Workflow Pipe -> (Pipe, WorkflowState)
buildTree wf = runState wf emptyWorkflowState
