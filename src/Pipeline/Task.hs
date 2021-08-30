{-# LANGUAGE BangPatterns #-}
{-|
Module      : Pipeline.Task
Description : To create tasks
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

Contains the tools needed to create, interact with and stop a network.
-}
module Pipeline.Task
  (
  -- * Creating a Task
  -- $task-description
    task
  ,
  -- * Smart Constructors
    multiInputTask
  , functionTask
  ,
  -- * Misc
    HList(..)
  , HList'(..)
  , ExceptT
  ) where

import           Control.DeepSeq                           (NFData, deepseq)
import           Control.Exception.Lifted                  (SomeException)
import           Control.Monad.Except                      (ExceptT)
import           Control.Monad.Trans                       (lift)
import           Pipeline.Internal.Common.HList            (HList (..),
                                                            HList' (..))
import           Pipeline.Internal.Common.IFunctor         (IFix5 (..))
import           Pipeline.Internal.Common.IFunctor.Modular ((:<:) (..))
import           Pipeline.Internal.Common.Nat              (Nat (..))
import           Pipeline.Internal.Common.TypeList         (Apply, Length)
import           Pipeline.Internal.Core.CircuitAST         (Circuit, Task (..))
import           Pipeline.Internal.Core.DataStore          (DataStore (..),
                                                            DataStore' (..))

{-|
This allows a function with multiple inputs to be converted into a 'Task'.
-}
multiInputTask
  :: (DataStore' fs as, DataStore g b, Eq (g b), NFData b)
  => (HList as -> b) -- ^ The function to execute
  -> Circuit fs as '[g] '[b] (Length fs)
multiInputTask f = IIn5
  (inj
    (Task
      (\sources sink -> do
        input <- lift (fetch' sources)
        let outputValue   = f input
            !outputValue' = outputValue `deepseq` outputValue
        lift (save sink outputValue')
      )
    )
  )


{-|
This allows a single @a -> b@ to be converted into a 'Task'.
-}
functionTask
  :: (DataStore f a, DataStore g b, Eq (g b), Eq a, Eq (f a), NFData b)
  => (a -> b) -- ^ The function to execute
  -> Circuit '[f] '[a] '[g] '[b] ( 'Succ 'Zero)
-- It is okay to pattern match the hlist to just one value, as the type states that it only consumes one element.
functionTask f = multiInputTask (\(HCons inp HNil) -> f inp)


-- $task-description
-- A 'Task', is one of the key components of this system.
-- They will be converted into threads in the process network.
--
-- The function a 'Task' executes has a specific type and a task will need to do multiple things.
--
-- The first agument will be a 'HList'' containing all the 'DataStore's, to be fetched from.
-- The second argument is the \"empty\" 'DataStore's or \"pointers\" to a storage location, where the result should be saved.
--
-- A task should:
--
--    * use 'fetch'' to retrieve all the inputs from the 'DataStore's.
--
--    * use 'save'' to save the outputs to the 'DataStore's, this should take the second argument given to the 'Task'.


-- | Constructor for a task
task
  :: (DataStore' fs as, DataStore g b, Eq (g b), NFData (g b))
  => (HList' fs as -> g b -> ExceptT SomeException IO ())  -- ^ The function a Task will execute.
  -> Circuit fs as '[g] '[b] (Length fs)
task f = IIn5 (inj (Task f))
