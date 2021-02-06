{-# LANGUAGE GADTs, RankNTypes #-}
module Pipeline.Backend.GraphMachine (

  
) where

-- import Data.Typeable (Typeable(..), cast)
-- import Data.List (elemIndex)
-- import Pipeline.Core.Task (Task, DataSource, VariableStore(..), functionTask)

-- type Edge = (Int, Int) -- (from, to) use index from array of nodes
-- data Graph a = Graph [a] [Edge]


-- data Node = forall i j b. (Typeable i, Typeable j, Typeable b) => TaskNode (Task i b j)
--           | forall m a i. (Typeable i, Typeable m, Typeable a, Monad m, DataSource m a i) => DataNode (m (a i))  deriving Typeable

-- -- Graph with just one node inside it. Stores a Task. 
-- -- exampleGraph :: Gr TaskNode ()
-- -- exampleGraph = ([], 1, Data (return (Var 1) :: IO (VariableStore Int)), [] :: [((), Node)]) & empty


-- exampleGraph' :: Graph Node
-- exampleGraph' = Graph [DataNode (return (Var 1) :: IO (VariableStore Int))] []


-- atobGraph :: Graph Node
-- atobGraph = Graph [
--   DataNode (return (Var 1) :: IO (VariableStore Int)), -- index 0
--   TaskNode (functionTask ((+1) :: Int -> Int)) -- index 1
--   ] [(0, 1)] -- Connect node 0 to node 1.   0 --> 1


-- makeStep :: [Int] -> Graph Node -> (Int, Graph Node)
-- makeStep [] _ = error "unable to make a step"
-- makeStep (uv:uvs) (Graph ns es)
--   | isUnblocked = (uv, Graph ns' es)
--   | otherwise = makeStep uvs (Graph ns es)
--     where
--       connectedEdges :: [Edge]
--       connectedEdges = filter (\(_, to) -> to == uv) es
--       parentNodes :: [Int]
--       parentNodes = map fst connectedEdges
--       isUnblocked :: Bool
--       isUnblocked = foldr (
--         \n unblocked -> (
--           case ns!!n of
--             TaskNode _ -> False
--             DataNode _ -> True) && unblocked
--         ) True parentNodes
--       ns' :: [Node]
--       ns' = foldr f [] (zip [0..] ns) -- replace uv with DataNode containing the DataSource.
--       f :: (Int, Node) -> [Node] -> [Node]
--       f (i, n) ns
--         | i == uv   = computeNode i n : ns
--         | otherwise =  n:ns
--       computeNode :: Int -> Node -> Node
--       computeNode i (TaskNode t) =
--         let parents = map (ns!!) parentNodes :: [Node]
--             firstParent = head parents :: Node
--         in case firstParent of
--           TaskNode _ -> error "shouldn't have a task node here"
--           DataNode d -> case cast d of
--             Nothing -> error ""
--             Just d1 -> case cast t of
--               Nothing -> error ""
--               Just t1 -> DataNode (t1 d1 Empty)

