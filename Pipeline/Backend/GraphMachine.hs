{-# LANGUAGE GADTs, RankNTypes #-}
module Pipeline.Backend.GraphMachine (

  
) where

import Pipeline.Core.Task (Task, DataSource, VariableStore(..), functionTask)
-- import Data.Graph.Inductive (Gr, empty, (&), Node)

type Edge = (Int, Int) -- (from, to) use index from array of nodes
data Graph a = Graph [a] [Edge]


data Node = forall a b. TaskNode (Task a b) | forall m a i. (Monad m, DataSource m a i) => DataNode (m (a i))


-- Graph with just one node inside it. Stores a Task. 
-- exampleGraph :: Gr TaskNode ()
-- exampleGraph = ([], 1, Data (return (Var 1) :: IO (VariableStore Int)), [] :: [((), Node)]) & empty


exampleGraph' :: Graph Node
exampleGraph' = Graph [DataNode (return (Var 1) :: IO (VariableStore Int))] []


atobGraph :: Graph Node
atobGraph = Graph [
  DataNode (return (Var 1) :: IO (VariableStore Int)), -- index 0
  TaskNode (functionTask ((+1) :: Int -> Int)) -- index 1
  ] [(0, 1)] -- Connect node 0 to node 1.   0 --> 1


runGraph :: Graph Node -> Graph Node 
runGraph = undefined
