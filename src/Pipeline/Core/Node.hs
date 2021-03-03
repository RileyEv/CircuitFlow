module Pipeline.Core.Node where

import Pipeline.Core.Task (TaskWrap)
import Pipeline.Core.DataStore (DataWrap)
import Pipeline.Core.Graph (TreeF(..))
import Pipeline.Core.IFunctor (IFix(..), icata)

{-|
  Datatype to be stored in the representation of a pipeline
-}
data Node = TaskNode    TaskWrap
          | DataNode    DataWrap
          -- | PointerNode Pointer

-- |Used to extract leafs in a tree after the execution of the pipeline is complete.
extractLeafs :: (IFix TreeF) Node -> [Node]
extractLeafs = icata alg
  where
    alg (TreeF x []) = [x]
    alg (TreeF _ xs) = concat xs
