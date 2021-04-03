module Pipeline.Task (
  multiInputTask,
  functionTask,
  Task(..),
  IFix7(..),
  (:<:)(..),
  HList(..),
) where

import Pipeline.Internal.Core.DataStore (DataStore'(..), DataStore(..))
import Pipeline.Internal.Core.CircuitAST (Task(..))
import Pipeline.Internal.Common.IFunctor (IFix7(..))
import Pipeline.Internal.Common.IFunctor.Modular ((:<:)(..))
import Pipeline.Internal.Common.HList (HList(..), IOList(..))
import Pipeline.Internal.Common.Nat (Nat(..))
import Pipeline.Internal.Common.TypeList (Length, Apply)

{-|
  This allows a function to be converted into a Task. 
-}
multiInputTask :: (DataStore' fs as,
                    DataStore g b, Task :<: iF) => (HList as -> b) -> g b -> IFix7 iF fs as (Apply fs as) '[g] '[b] '[g b] (Length fs)
multiInputTask f output = IIn7 (inj (Task (\sources sink -> do
  input <- (hSequence . fetch') sources
  save sink (f input)) output))
  
functionTask :: (DataStore f a, DataStore g b, Task :<: iF) => (a -> b) -> g b -> IFix7 iF '[f] '[a] '[f a] '[g] '[b] '[g b] ('Succ 'Zero)
-- It is okay to pattern match the hlist to just one value, as the type states that it only consumes one element.
functionTask f = multiInputTask (\(HCons inp HNil) -> f inp)

hSequence :: IOList as -> IO (HList as)
hSequence IONil = return HNil
hSequence (IOCons x xs) = do
  x' <- x
  xs' <- hSequence xs
  return $ x' `HCons` xs'

