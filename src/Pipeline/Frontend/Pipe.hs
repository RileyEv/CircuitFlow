module Pipeline.Frontend.Pipe where

import Pipeline.Core.Task (Task, functionTask)
import Pipeline.Core.DataStore (DataSource, VariableStore(..), IOStore(..), FileStore(..))
import Pipeline.Backend.GraphMachine (Node(..), TreeF(..), processTree)
import Pipeline.Core.IFunctor (IFunctor4(..), IFix4(..), icata4, C4(..), IFix(..), icata, cata)


import qualified Data.Map as M (Map, insert, empty, lookup)
import Control.Monad.State (State, runState, get, put)
import Data.Typeable (Typeable)


data ChainF f g a h b where
  ProcessF :: (DataSource g a, DataSource h b) => PID -> ChainF f g a h b
  JoinF    :: (DataSource g a, DataSource h b, DataSource i c) => f g a h b -> f h b i c -> ChainF f g a i c

instance IFunctor4 ChainF where
  imap4 _ (ProcessF pid) = ProcessF pid
  imap4 f (JoinF x y) = JoinF (f x) (f y)

type Chain f a g b = IFix4 ChainF f a g b
type ChainAlg f a b c d = ChainF f a b c d -> f a b c d

data Pipe where
  Pipe :: forall f a g b. (DataSource f a, DataSource g b) => Chain f a g b -> Pipe
  And  :: Pipe -> Pipe -> Pipe

-- |Used to join multiple chains together into a pipeline.
(&) :: Pipe -> Pipe -> Pipe
x & y = And x y

infixr 8 &

-- Look at Control.Arrow? Does this apply?

-- |This operator is used to join tasks together to form chains.
(>>>) :: (DataSource f a, DataSource g b, DataSource h c) => Chain f a g b -> Chain g b h c -> Chain f a h c
x >>> y = IIn4 (JoinF x y)

infixr >>> 

-- A problem i encounted is described below. Left here for future reference.
-- testPipeline1 :: Pipe
-- testPipeline1 =  Pipe $
--   readIOTask >> plus1Task >> plus1Task >> plus1Task >> showFileTask "testfiles/testPipeline1.out"  
-- --                         \
-- --                          >> plus1Task >> showFileTask "testfiles/testPipeline1-1.out"
-- Above demonstrates a problem with this method.
-- How do you know when a branch occurs if you use the same task multiple times.
-- One method would to be to require new types for each task that is used.
--
-- Another solution as suggested by Sam is to use PIDs
--
-- A task is registered and then given a unique PID which can be returned from the register task function.
-- Combine tasks with pids

-- |A unique identifier for each process in a pipeline
type PID = Int

-- |Required to store tasks of differing types in a single 'Map'. Uses existential types.
data TaskWrap = forall f a g b. (DataSource f a, DataSource g b, Typeable f, Typeable a, Typeable g, Typeable b) => TaskWrap (Task f a g b)

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
      Just (TaskWrap n) -> TaskNode n
      Nothing -> error "Not in the map")

-- |Runs a pipeline with the given input 'DataSource'
runWorkflow :: (DataSource f a, Typeable f, Typeable a) => Workflow Pipe -> f a -> IO ((IFix TreeF) Node)
runWorkflow wf inp = do
  let (p, s) = runState wf (WorkflowState (-1) M.empty)
  let pidTree = pipeToTree p
  let nodeTree = pidTreeToNodeTree (tasks s) pidTree
  processTree nodeTree inp

-- |Used to extract leafs in a tree after the execution of the pipeline is complete.
extractLeafs :: (IFix TreeF) Node -> [Node]
extractLeafs = icata alg
  where
    alg (TreeF x []) = [x]
    alg (TreeF _ xs) = concat xs

