{-# LANGUAGE DataKinds #-}
module Pipeline.Core.Task (
  Task(..),
  TaskWrap(..),
  functionTask,
  functionTaskF,
  multiInputFunctionTask,
  multiInputFunctionTaskF,
) where

import Data.Typeable (Typeable)
import Pipeline.Core.DataStore (DataSource(..), DataSource'(..), Apply, HList(..), IOList(..))



{-|
  The main wrapping data type for a function. This makes working with the function type easier. 
-}
data Task fs as g b = (
  DataSource' fs as (Apply fs as),
  DataSource g b,
  Typeable (Apply fs as),
  Typeable fs, Typeable g,
  Typeable as, Typeable b)
  => Task (HList (Apply fs as) -> g b -> IO (g b)) (g b)



data TaskF iF fas gb = forall fs as g b. (
  fas ~ Apply fs as,
  gb ~ Apply '[g] '[b],
  DataSource' fs as (Apply fs as),
  DataSource g b,
  Typeable (Apply fs as),
  Typeable fs, Typeable g,
  Typeable as, Typeable b)
  => TaskF (HList (Apply fs as) -> g b -> IO (g b)) (g b)

-- |Required to store tasks of differing types in a single 'Map'. Uses existential types.
data TaskWrap = forall fs as g b. (
  DataSource' fs as (Apply fs as), DataSource g b,
  Typeable (Apply fs as),
  Typeable fs, Typeable as, Typeable g, Typeable b) => TaskWrap (Task fs as g b)


{-|
  This allows a function to be converted into a Task. 
-}
multiInputFunctionTask :: (DataSource' fs as (Apply fs as), DataSource g b, Typeable as, Typeable b, Typeable fs, Typeable g, Typeable (Apply fs as)) => (HList as -> b) -> g b -> Task fs as g b 
multiInputFunctionTask f = Task (\sources sink -> do
  input <- (hSequence . fetch') sources
  save sink (f input))

functionTask :: (DataSource f a, DataSource g b, Typeable f, Typeable a, Typeable g, Typeable b) => (a -> b) -> g b -> Task '[f] '[a] g b
-- It is okay to pattern match the hlist to just one value, as the type states that it only consumes one element.
functionTask f = multiInputFunctionTask (\(HCons inp HNil) -> f inp)

multiInputFunctionTaskF :: (DataSource' fs as (Apply fs as),
                            DataSource g b,
                            Typeable as, Typeable b,
                            Typeable fs, Typeable g,
                            Typeable (Apply fs as)) => (HList as -> b) -> g b -> TaskF iF (Apply fs as) '[g b] 
multiInputFunctionTaskF f = TaskF (\sources sink -> do
  input <- (hSequence . fetch') sources
  save sink (f input))
  
functionTaskF :: (DataSource f a, DataSource g b, Typeable f, Typeable a, Typeable g, Typeable b) => (a -> b) -> g b -> TaskF iF '[f a] '[g b]
-- It is okay to pattern match the hlist to just one value, as the type states that it only consumes one element.
functionTaskF f = multiInputFunctionTaskF (\(HCons inp HNil) -> f inp)

hSequence :: IOList as -> IO (HList as)
hSequence IONil = return HNil
hSequence (IOCons x xs) = do
  x' <- x
  xs' <- hSequence xs
  return $ x' `HCons` xs'
