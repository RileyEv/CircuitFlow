module Pipeline.Frontend.Pipe where

import Prelude hiding ((>>))

import Pipeline.Core.Task (Task, functionTask)
import Pipeline.Core.DataStore (DataSource, VariableStore(..), IOStore(..), FileStore(..))


data Chain f a g b where
  Chain :: (DataSource f a, DataSource g b) => Task f a g b -> Chain f a g b
  Join  :: (DataSource f a, DataSource g b, DataSource h c) => Chain f a g b -> Chain g b h c -> Chain f a h c

data Pipe where
  Pipe :: forall f a g b. (DataSource f a, DataSource g b) => Chain f a g b -> Pipe
  And  :: Pipe -> Pipe -> Pipe

(&) :: (DataSource f a, DataSource g b, DataSource h c, DataSource i d) => Chain f a g b -> Chain h c i d -> Pipe
x & y = And (Pipe x) (Pipe y)

-- Look at Control.Arrow? Does this apply?

(>>) :: (DataSource f a, DataSource g b, DataSource h c) => Chain f a g b -> Chain g b h c -> Chain f a h c
x >> y = Join x y

infixr >>


readIOTask :: Chain IOStore String VariableStore Int
readIOTask = Chain (functionTask (read :: String -> Int) Empty)

plus1Task :: Chain VariableStore Int VariableStore Int
plus1Task = Chain (functionTask (+ (1 :: Int)) Empty)

showFileTask :: FilePath -> Chain VariableStore Int FileStore String
showFileTask f = Chain (functionTask (show :: Int -> String) (FileStore f))


testPipeline1 :: Pipe
testPipeline1 =  Pipe $
  readIOTask >> plus1Task >> plus1Task >> plus1Task >> showFileTask "testfiles/testPipeline1.out"  
--                         \
--                          >> plus1Task >> showFileTask "testfiles/testPipeline1-1.out"
-- Above demonstrates a problem with this method.
-- How do you know when a branch occurs if you use the same task multiple times.
-- One method would to be to require new types for each task that is used. 

