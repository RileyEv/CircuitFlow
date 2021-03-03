
module Pipeline.Core.Task (
  Task(..),
  TaskWrap(..),
  functionTask,
) where

import Data.Typeable (Typeable)
import Pipeline.Core.DataStore (DataSource(..))


-- f a only has to be a source as it is never saved to
-- g b has to be both as this task saves to it and the next task will read

{-|
  The main wrapping data type for a function. This makes working with the function type easier. 
-}
data Task f a g b = (DataSource f a, DataSource g b, Typeable f, Typeable g, Typeable a, Typeable b) => Task (f a -> g b -> IO (g b)) (g b)


-- |Required to store tasks of differing types in a single 'Map'. Uses existential types.
data TaskWrap = forall f a g b. (
  DataSource f a, DataSource g b,
  Typeable f, Typeable a, Typeable g, Typeable b) => TaskWrap (Task f a g b)

{-|
  This allows a function to be converted into a Task. 
-}
functionTask :: (DataSource f a, DataSource g b, Typeable a, Typeable b, Typeable f, Typeable g) => (a -> b) -> g b -> Task f a g b 
functionTask f = Task (\source sink -> do
  input <- fetch source
  save sink (f input))
