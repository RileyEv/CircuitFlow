module Pipeline where

import Pipeline.Core.DataStore (DataSource)
import Pipeline.Core.IFunctor (IFix)
import Pipeline.Core.Graph (TreeF)
import Pipeline.Core.Node (Node)
import Pipeline.Frontend.Workflow (Workflow, buildTree, pipeToTree, tasks, pidTreeToNodeTree)
import Pipeline.Frontend.Pipe (Pipe)
import Pipeline.Frontend.Verify (verify)
import Pipeline.Backend.GraphMachine (processTree)

import Data.Typeable (Typeable)

-- |Runs a pipeline with the given input 'DataSource'
runWorkflow :: (DataSource f a, Typeable f, Typeable a) => Workflow Pipe -> f a -> IO ((IFix TreeF) Node)
runWorkflow wf inp = do
  let (p, s) = buildTree wf
  let pidTree = pipeToTree p
  verify pidTree (tasks s)
  let nodeTree = pidTreeToNodeTree (tasks s) pidTree
  processTree nodeTree inp
