{-# LANGUAGE DataKinds, PolyKinds #-}
module Pipeline.Core.Task (
  TaskF(..),
  functionTaskF,
  multiInputFunctionTaskF,
) where

import Data.Typeable (Typeable)
import Pipeline.Core.DataStore (DataSource(..), DataSource'(..), Apply, IOList(..))
import Pipeline.Core.HList
import Pipeline.Core.Modular ((:<:)(..))
import Pipeline.Core.IFunctor (IFix4(..), IFunctor4(..))


{-|
  The main wrapping data type for a function. This makes working with the function type easier. 
-}
data TaskF (iF :: [* -> *] -> [*] -> [* -> *] -> [*] -> *) (fs :: [* -> *]) (as :: [*]) (g :: [* -> *]) (b :: [*]) = forall g' b'. (
  g ~ '[g'], b ~ '[b'],
  DataSource' fs as,
  DataSource g' b')
  => TaskF (HList' fs as -> g' b' -> IO (g' b')) (g' b')

instance IFunctor4 TaskF where
  imap4 _ (TaskF f output) = TaskF f output


{-|
  This allows a function to be converted into a Task. 
-}
multiInputFunctionTaskF :: (DataSource' fs as,
                            DataSource g b, TaskF :<: iF) => (HList as -> b) -> g b -> IFix4 iF fs as '[g] '[b] 
multiInputFunctionTaskF f output = IIn4 (inj (TaskF (\sources sink -> do
  input <- (hSequence . fetch') sources
  save sink (f input)) output))
  
functionTaskF :: (DataSource f a, DataSource g b, TaskF :<: iF) => (a -> b) -> g b -> IFix4 iF '[f] '[a] '[g] '[b]
-- It is okay to pattern match the hlist to just one value, as the type states that it only consumes one element.
functionTaskF f = multiInputFunctionTaskF (\(HCons inp HNil) -> f inp)

hSequence :: IOList as -> IO (HList as)
hSequence IONil = return HNil
hSequence (IOCons x xs) = do
  x' <- x
  xs' <- hSequence xs
  return $ x' `HCons` xs'
