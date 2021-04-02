{-# LANGUAGE DataKinds, PolyKinds #-}
module Pipeline.Core.Task (
  TaskF(..),
  functionTaskF,
  multiInputFunctionTaskF,
) where

import Data.Typeable (Typeable)
import Pipeline.Core.DataStore (DataSource(..), DataSource'(..), Apply, IOList(..))
import Pipeline.Core.Nat (Length, (:=), Nat(..))
import Pipeline.Core.HList
import Pipeline.Core.Modular ((:<:)(..))
import Pipeline.Core.IFunctor (IFix7(..), IFunctor7(..))


{-|
  The main wrapping data type for a function. This makes working with the function type easier. 
-}
data TaskF (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> Nat -> *)
           (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
           (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) (ninputs :: Nat) = forall g' b'. (
  Length outputsS := Succ Zero ~ 'True,
  outputsS ~ '[g'], outputsT ~ '[b'], outputsA ~ '[g' b'],
  DataSource' inputsS inputsT,
  DataSource g' b')
  => TaskF (HList' inputsS inputsT -> g' b' -> IO (g' b')) (g' b')

instance IFunctor7 TaskF where
  imap7 _ (TaskF f output) = TaskF f output


{-|
  This allows a function to be converted into a Task. 
-}
multiInputFunctionTaskF :: (DataSource' fs as,
                            DataSource g b, TaskF :<: iF) => (HList as -> b) -> g b -> IFix7 iF fs as (Apply fs as) '[g] '[b] '[g b] (Length fs)
multiInputFunctionTaskF f output = IIn7 (inj (TaskF (\sources sink -> do
  input <- (hSequence . fetch') sources
  save sink (f input)) output))
  
functionTaskF :: (DataSource f a, DataSource g b, TaskF :<: iF) => (a -> b) -> g b -> IFix7 iF '[f] '[a] '[f a] '[g] '[b] '[g b] ('Succ 'Zero)
-- It is okay to pattern match the hlist to just one value, as the type states that it only consumes one element.
functionTaskF f = multiInputFunctionTaskF (\(HCons inp HNil) -> f inp)

hSequence :: IOList as -> IO (HList as)
hSequence IONil = return HNil
hSequence (IOCons x xs) = do
  x' <- x
  xs' <- hSequence xs
  return $ x' `HCons` xs'
