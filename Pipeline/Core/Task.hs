{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances, GADTs, KindSignatures, DataKinds, FlexibleContexts #-}
module Pipeline.Core.Task (
  Task(..)
) where

import Data.Typeable (Typeable(..), cast, gcast)


-- a i only has to be a source as it is never saved to
-- b j has to be both as this task saves to it and the next task will read

data Task a b = (DataSource VariableStore a, DataSink VariableStore b, DataSource VariableStore b, Typeable a, Typeable b) => Task (VariableStore a -> VariableStore b -> IO (VariableStore b))



class (Typeable a, Typeable f) => DataSource f a where
  fetch :: f a -> IO a

class (Typeable a, Typeable f) => DataSink f a where
  -- First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  save :: f a -> a -> IO (f a) 


-- Basic DataSource/Sink

data VariableStore a = Var a | Empty deriving Typeable

instance Typeable a => DataSource VariableStore a where
  fetch (Var x) = return x
  fetch Empty   = error "empty source"

instance Typeable a => DataSink VariableStore a where
  save _ x = return (Var x)




data Node = forall a b. (Typeable a, Typeable b) =>  TaskNode (Task a b)
          | forall a. (DataSource VariableStore a, Typeable a) => DataNode (VariableStore a)
-- data Node = forall i b j. (Typeable b, Typeable i, Typeable j) =>  TaskNode (Task i b j)
--           | forall a i. (DataSource a i, Typeable a, Typeable i) => DataNode (a i)


-- Lets start with a simple graph represented with a list. ie a -> b -> .. -> x
processGraph :: (DataSource VariableStore a, Typeable a) => [Node] -> VariableStore a -> IO [Node]
processGraph ns firstD = foldl f (return [DataNode firstD]) ns
  where
    f :: IO [Node] -> Node -> IO [Node]
    f ds' (TaskNode (Task t)) = do
      ds <- ds'
      let dn@(DataNode d) = last ds
          nextd' = case gcast d of
            Just d1 -> t d1 Empty
            Nothing -> error "problem"
      nextd <- nextd'
      return (ds ++ [dn, DataNode nextd])

    f _ _ = error "How did it get here..."




-- Example typeable with existential types

data Currency = IGBP | USD | AUD
data Money (currency :: Currency) = Money Int
data MoneyEx = forall x . Typeable x => MoneyEx (Money x)


moneyAdd :: Money c -> Money c -> Money c
moneyAdd (Money a) (Money b) = Money (a + b)

moneyExAdd :: MoneyEx -> MoneyEx -> MoneyEx
moneyExAdd (MoneyEx a) (MoneyEx b) = case cast a of
  Just a1 -> MoneyEx (moneyAdd a1 b)
  Nothing -> error ""
