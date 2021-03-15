{-# LANGUAGE DataKinds, PolyKinds #-}
module Pipeline.Core.Task (
  TaskF(..),
  functionTaskF,
  multiInputFunctionTaskF,
) where

import Data.Typeable (Typeable)
import Pipeline.Core.DataStore (DataSource(..), DataSource'(..), Apply, HList(..), IOList(..))
import Pipeline.Core.Modular ((:<:)(..))
import Pipeline.Core.IFunctor (IFix2(..), IFunctor2(..))


{-|
  The main wrapping data type for a function. This makes working with the function type easier. 
-}
data TaskF (iF :: [*] -> [*] -> *) (fas :: [*]) (gb :: [*]) = forall fs as g b. (
  fas ~ Apply fs as,
  gb ~ Apply '[g] '[b],
  DataSource' fs as (Apply fs as),
  DataSource g b,
  Typeable (Apply fs as),
  Typeable fs, Typeable g,
  Typeable as, Typeable b)
  => TaskF (HList (Apply fs as) -> g b -> IO (g b)) (g b)

instance IFunctor2 TaskF where
  imap2 _ (TaskF f output) = TaskF f output


{-|
  This allows a function to be converted into a Task. 
-}
multiInputFunctionTaskF :: (DataSource' fs as (Apply fs as),
                            DataSource g b,
                            Typeable as, Typeable b,
                            Typeable fs, Typeable g,
                            Typeable (Apply fs as), TaskF :<: iF) => (HList as -> b) -> g b -> IFix2 iF (Apply fs as) '[g b] 
multiInputFunctionTaskF f output = IIn2 (inj (TaskF (\sources sink -> do
  input <- (hSequence . fetch') sources
  save sink (f input)) output))
  
functionTaskF :: (DataSource f a, DataSource g b, Typeable f, Typeable a, Typeable g, Typeable b, TaskF :<: iF) => (a -> b) -> g b -> IFix2 iF '[f a] '[g b]
-- It is okay to pattern match the hlist to just one value, as the type states that it only consumes one element.
functionTaskF f = multiInputFunctionTaskF (\(HCons inp HNil) -> f inp)

hSequence :: IOList as -> IO (HList as)
hSequence IONil = return HNil
hSequence (IOCons x xs) = do
  x' <- x
  xs' <- hSequence xs
  return $ x' `HCons` xs'
