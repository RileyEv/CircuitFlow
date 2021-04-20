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
  ) where

import           Control.DeepSeq                           (NFData, deepseq)
import           Control.Exception.Lifted                  (SomeException)
import           Control.Monad.Except                      (ExceptT)
import           Control.Monad.Trans                       (lift)
import           Pipeline.Internal.Common.HList            (HList (..),
                                                            HList' (..),
                                                            IOList (..))
import           Pipeline.Internal.Common.IFunctor         (IFix7 (..))
import           Pipeline.Internal.Common.IFunctor.Modular ((:<:) (..))
import           Pipeline.Internal.Common.Nat              (Nat (..))
import           Pipeline.Internal.Common.TypeList         (Apply, Length)
import           Pipeline.Internal.Core.CircuitAST         (Circuit, Task (..))
import           Pipeline.Internal.Core.DataStore          (DataStore (..),
                                                            DataStore' (..))
import           Pipeline.Internal.Core.UUID               (UUID)

{-|
This allows a function with multiple inputs to be converted into a 'Task'.
-}
multiInputTask
  :: (DataStore' fs as, DataStore g b, Eq (g b), Show (g b), NFData (g b), NFData b)
  => (HList as -> b) -- ^ The function to execute
  -> g b             -- ^ The output 'DataStore'
  -> Circuit fs as (Apply fs as) '[g] '[b] '[g b] (Length fs)
multiInputTask f output = IIn7
  (inj
    (Task
      (\uuid sources sink -> do
        input <- lift ((hSequence . fetch' uuid) sources)
        let outputValue   = f input
            !outputValue' = outputValue `deepseq` outputValue
        lift (save uuid sink outputValue')
      )
      output
    )
  )

{-|
This allows a single @a -> b@ to be converted into a 'Task'.
-}
functionTask
  :: (DataStore f a, DataStore g b, Eq (g b), Show (g b), NFData (g b), NFData b)
  => (a -> b) -- ^ The function to execute
  -> g b      -- ^ The output 'DataStore'
  -> Circuit '[f] '[a] '[f a] '[g] '[b] '[g b] ( 'Succ 'Zero)
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
  :: (DataStore' fs as, DataStore g b, Eq (g b), Show (g b), NFData (g b))
  => (UUID -> HList' fs as -> g b -> ExceptT SomeException IO (g b))  -- ^ The function a Task will execute.
  -> g b                                -- ^ The output 'DataStore'
  -> Circuit fs as (Apply fs as) '[g] '[b] '[g b] (Length fs)
task f out = IIn7 (inj (Task f out))


hSequence :: IOList as -> IO (HList as)
hSequence IONil         = return HNil
hSequence (IOCons x xs) = do
  x'  <- x
  xs' <- hSequence xs
  return $ x' `HCons` xs'
