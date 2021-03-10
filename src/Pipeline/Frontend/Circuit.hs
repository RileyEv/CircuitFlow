{-# LANGUAGE MultiParamTypeClasses
           , TypeFamilyDependencies
           , PolyKinds
           , DataKinds
           , AllowAmbiguousTypes #-}
module Pipeline.Frontend.Circuit where

import Pipeline.Core.DataStore
-- import Pipeline.Core.Task
import Data.Typeable (Typeable)
import Prelude hiding (id, replicate)

{-|
  The main wrapping data type for a function. This makes working with the function type easier. 
-}
data Task fs as g b = (
  DataSource' fs as (Apply fs as),
  DataSource g b,
  Typeable fs, Typeable g,
  Typeable as, Typeable b)
  => Task (HList (Apply fs as) -> g b -> IO (g b)) (g b)


-- |Required to store tasks of differing types in a single 'Map'. Uses existential types.
-- data TaskWrap = forall f a g b. (
--   DataSource f a, DataSource g b,
--   Typeable f, Typeable a, Typeable g, Typeable b) => TaskWrap (Task f a g b)

{-|
  This allows a function to be converted into a Task. 
-}
multiInputFunctionTask :: (DataSource' fs as (Apply fs as), DataSource g b, Typeable as, Typeable b, Typeable fs, Typeable g) => (HList as -> b) -> g b -> Task fs as g b 
multiInputFunctionTask f = Task (\sources sink -> do
  input <- (hSequence . fetch') sources
  save sink (f input))

functionTask :: (DataSource f a, DataSource g b, Typeable f, Typeable a, Typeable g, Typeable b) => (a -> b) -> g b -> Task '[f] '[a] g b
-- It is okay to pattern match the hlist to just one value, as the type states that it only consumes one element.
functionTask f = multiInputFunctionTask (\(HCons inp HNil) -> f inp)

hSequence :: IOList as -> IO (HList as)
hSequence IONil = return HNil
hSequence (IOCons x xs) = do
  x' <- x
  xs' <- hSequence xs
  return $ x' `HCons` xs'



data IOList (xs :: [*]) where
  IOCons :: IO x -> IOList xs -> IOList (x ': xs)
  IONil :: IOList '[]

data HList (xs :: [*]) where
  HCons :: x -> HList xs -> HList (x ': xs)
  HNil :: HList '[]
  

type family HAppendListR (l1 :: [k]) (l2 :: [k]) where
  HAppendListR '[] l = l
  HAppendListR (e ': l) l' = e ': HAppendListR l l'


type family Apply (fs :: [* -> *]) (as :: [*]) = fas | fas -> fs as where
  Apply '[] '[] = '[]
  Apply (f ': fs) (a ': as) = f a ': Apply fs as


class (xs ~ Apply fs as) => DataSource' (fs :: [* -> *]) (as :: [*]) (xs :: [*]) where
  -- | Fetch the value stored in the 'DataSource'
  -- fetch :: f a -> IO a
  fetch' :: HList xs -> IOList as
  -- | Save a value into the 'DataStore'
  --   First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  -- save :: f a -> a -> IO (f a)
  save' :: HList xs -> HList as -> IOList xs

-- -- for the user to define.
-- class DataSource f a where
--   fetch :: f a -> IO a
--   save :: f a -> a -> IO (f a)


instance {-# OVERLAPPING #-} (x ~ f a, DataSource f a) => DataSource' '[f] '[a] '[x] where
  fetch' (HCons x HNil) = IOCons (fetch x) IONil
  save' = undefined

instance (x ~ f a, DataSource f a, DataSource' fs as xs) => DataSource' (f ': fs) (a ': as) (x ': xs) where
  fetch' (HCons x xs) = IOCons (fetch x) (fetch' xs)
  save' = undefined



data Circuit i o where
  Id        :: (DataSource' '[f] '[a] '[f a]) => Circuit (Apply '[f] '[a]) (Apply '[f] '[a])
  Apply     :: (DataSource' fs as (Apply fs as), DataSource' '[g] '[b] '[g b] ) => Task fs as g b -> Circuit (Apply fs as) (Apply '[g] '[b])
  Replicate :: (DataSource' '[f] '[a] '[f a]) => Circuit (Apply '[f] '[a]) (Apply '[f, f] '[a, a])
  Then      :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs))
    => Circuit (Apply fs as) (Apply gs bs)
    -> Circuit (Apply gs bs) (Apply hs cs)
    -> Circuit (Apply fs as) (Apply hs cs)
  Beside    :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs), DataSource' is ds (Apply is ds))
    => Circuit (Apply fs as) (Apply gs bs)
    -> Circuit (Apply hs cs) (Apply is ds)
    -> Circuit (Apply (HAppendListR fs hs) (HAppendListR as cs)) (Apply (HAppendListR gs is) (HAppendListR bs ds))
  Swap      :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Circuit (Apply '[f, g] '[a, b]) (Apply '[g, f] '[b, a])
  DropL     :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Circuit (Apply '[f, g] '[a, b]) (Apply '[g] '[b])
  DropR     :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Circuit (Apply '[f, g] '[a, b]) (Apply '[f] '[a])
  


-- Smart Constructors

id        :: (DataSource' '[f] '[a] '[f a]) => Circuit (Apply '[f] '[a]) (Apply '[f] '[a])
apply     :: (DataSource' fs as (Apply fs as), DataSource' '[g] '[b] '[g b] ) => Task fs as g b -> Circuit (Apply fs as) (Apply '[g] '[b])
replicate :: (DataSource' '[f] '[a] '[f a]) => Circuit (Apply '[f] '[a]) (Apply '[f, f] '[a, a])
(<->)     :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs))
             => Circuit (Apply fs as) (Apply gs bs)
             -> Circuit (Apply gs bs) (Apply hs cs)
             -> Circuit (Apply fs as) (Apply hs cs)
(<|>)     :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs), DataSource' is ds (Apply is ds))
             => Circuit (Apply fs as) (Apply gs bs)
             -> Circuit (Apply hs cs) (Apply is ds)
             -> Circuit (Apply (HAppendListR fs hs) (HAppendListR as cs)) (Apply (HAppendListR gs is) (HAppendListR bs ds))
swap      :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Circuit (Apply '[f, g] '[a, b]) (Apply '[g, f] '[b, a])
dropL     :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Circuit (Apply '[f, g] '[a, b]) (Apply '[g] '[b])
dropR     :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Circuit (Apply '[f, g] '[a, b]) (Apply '[f] '[a])
id        = Id
apply     = Apply
replicate = Replicate
(<->)     = Then
(<|>)     = Beside
swap      = Swap
dropL     = DropL
dropR     = DropR

infixr 4 <->
infixr 5 <|>
  


-- This stuff is what I want


plus1Task :: Task '[VariableStore] '[Int] VariableStore Int
plus1Task = functionTask (+1) Empty

showTask :: Task '[VariableStore] '[Int] VariableStore String
showTask = functionTask show Empty

appendTask :: Task '[VariableStore, VariableStore] '[String, String] VariableStore String
appendTask = multiInputFunctionTask (\(HCons x (HCons y HNil)) -> x ++ y ) Empty


-- Example1
--   a  
--   |  
--  +1  
--  show
--   |  
--   b
example1 :: Circuit '[VariableStore Int] '[VariableStore String]
-- example = Then (Then Id (Apply plus1Task)) (Apply showTask)
example1 = id
          <->
          apply plus1Task
          <->
          apply showTask


-- Example 2
--     a
--    / \
--   +1 show
--    \ /
--     /
--    / \
--    | |
--    b c
example2 :: Circuit '[VariableStore Int] '[VariableStore String, VariableStore Int]
-- example2 = Then (Then Replicate (Beside (Apply plus1Task) (Apply showTask))) Swap
example2 = replicate
           <->
           apply plus1Task <|> apply showTask
           <->
           swap

-- Example 3
--   a      b
--   |     / \
--  +1    +1 show
--  show   \ /
--   |      /
--   |     / \
--   |    /  |
--   |   /   |
--   |  /    |
--   \ /     | 
--    /      |
--   / \     |
--   \ /     |
--   ++      |
--    |      |
--    c      d
example3 :: Circuit '[VariableStore Int, VariableStore Int] '[VariableStore String, VariableStore Int]
example3 = example1         <|> example2
           -- VariableStore String (1), VariableStore String (2), VariableStore Int
           <-> 
           swap             <|> id
           -- VariableStore String (2), VariableStore String (1), VariableStore Int
           <->
           apply appendTask <|> id
           -- VariableStore String (2 ++ 1), VariableStore Int


           





-- id :: DataSource f a => Circuit (f a) (f a)
-- id = ID

-- apply :: (DataSource f a, DataSource g b) => Task f a g b -> Circuit (f a) (g b)
-- apply = Apply

-- branch :: (DataSource f a) => Circuit (f a) (f a, f a)
-- branch = Branch

-- (<..>) :: (DataSource f a, DataSource g b, DataSource h c) => Circuit (f a) (g b) -> Circuit (g b) (h c) -> Circuit (f a) (h c)
-- (<..>) = Then

-- infixr 4 <..> 


-- (<&>) :: (DataSource f a, DataSource g b, DataSource h c, DataSource i d) => Circuit (f a) (g b) -> Circuit (h c) (i d) -> Circuit (f a, h c) (g b, i d)
-- (<&>) = Beside

-- infixr 5 <&>

-- swap :: (DataSource f a, DataSource g b) => Circuit (f a, g b) (g b, f a)
-- swap = Swap






-- example :: Circuit (VariableStore Int) (VariableStore String, VariableStore Int)
-- example = branch
--           <..>
--           apply plus1Task <&> id
--           <..>
--           apply plus1Task <&> apply showTask
--           <..>
--           swap

-- data VariableStore a = Var a | VEmpty

-- instance DataSource VariableStore a where
--   fetch (Var x) = return x
--   fetch VEmpty   = error "empty source"
--   save _ x = return (Var x)

-- instance DataSource (f :&&: g) a b where
--   fetch (x :&&: y) = do
--     x' <- fetch x 
--     y' <- fetch y
--     return (x', y')
--   save (x :&&: y) (x', y') = do
--     x'' <- save x x'
--     y'' <- save y y'
--     return x'' :&&: y''
  

-- example' :: Circuit (VariableStore Int) (VariableStore (String, Int))
-- example' = branch
--            <..>
--            (apply (+1) <..> apply (+1)) <&> apply (show :: Int -> String)
--            <..>
--            swap

  
-- example = Then (Then
--                  Branch
--                  (Apply (+1) `Beside` ID))
--                (Apply (+1) `Beside` Apply (+1))
