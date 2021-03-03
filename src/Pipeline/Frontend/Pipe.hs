module Pipeline.Frontend.Pipe where

import Pipeline.Backend.GraphMachine (Node(..)) -- remove

import Pipeline.Core.DataStore (DataSource)
import Pipeline.Core.IFunctor (IFunctor4(..), IFix4(..), IFix(..), icata)
import Pipeline.Core.Graph (TreeF(..))

import Pipeline.Frontend.PID (PID)




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



