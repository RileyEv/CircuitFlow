module Pipeline.Frontend.Circuit where

import Pipeline.Core.DataStore
import Pipeline.Core.Task
import Prelude hiding (id, replicate, (<>))


-- Main type of the DSL
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
(<>)      :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs), DataSource' is ds (Apply is ds))
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
(<>)      = Beside
swap      = Swap
dropL     = DropL
dropR     = DropR

-- TODO
split n   = undefined

infixr 4 <->
infixr 5 <>
  



-- Some example tasks
plus1Task :: Task '[VariableStore] '[Int] VariableStore Int
plus1Task = functionTask (+1) Empty

showTask :: Task '[VariableStore] '[Int] VariableStore String
showTask = functionTask show Empty

appendTask :: Task '[VariableStore, VariableStore] '[String, String] VariableStore String
appendTask = multiInputFunctionTask (\(HCons x (HCons y HNil)) -> x ++ y ) Empty


-- Example Circuits

-- Example1
--   a  
--   |  
--  +1
--   |
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
           apply plus1Task <> apply showTask
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
example3 = example1         <> example2
           -- VariableStore String (1), VariableStore String (2), VariableStore Int
           <-> 
           swap             <> id
           -- VariableStore String (2), VariableStore String (1), VariableStore Int
           <->
           apply appendTask <> id
           -- VariableStore String (2 ++ 1), VariableStore Int


-- a few rules to note...
-- replicate <-> dropL = id
-- replicate <-> dropR = id
-- replicate <-> apply f <> apply g <-> swap = replicate <-> apply g <> apply f
-- x <> y <-> swap <-> apply f = x <> y <-> apply (flip f)

-- There are probably more I will notice when using this.
-- I think example3 could be reduced so there are no swaps. Don't thing swap can be completely eliminated.
