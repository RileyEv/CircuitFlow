{-# LANGUAGE PolyKinds #-}
module Pipeline.Frontend.Circuit where

import Pipeline.Core.DataStore
import Pipeline.Core.Task
import Pipeline.Core.Modular ((:+:)(..), (:<:)(..))
import Pipeline.Core.IFunctor (IFix2(..), IFunctor2(..))
import Prelude hiding (id, replicate, (<>))


-- Main type of the DSL

-- Non-fixed version
-- data Circuit i o where
--   Id        :: (DataSource' '[f] '[a] '[f a]) => Circuit (Apply '[f] '[a]) (Apply '[f] '[a])
--   Apply     :: (DataSource' fs as (Apply fs as), DataSource' '[g] '[b] '[g b] ) => Task fs as g b -> Circuit (Apply fs as) (Apply '[g] '[b])
--   Replicate :: (DataSource' '[f] '[a] '[f a]) => Circuit (Apply '[f] '[a]) (Apply '[f, f] '[a, a])
--   Then      :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs))
--     => Circuit (Apply fs as) (Apply gs bs)
--     -> Circuit (Apply gs bs) (Apply hs cs)
--     -> Circuit (Apply fs as) (Apply hs cs)
--   Beside    :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs), DataSource' is ds (Apply is ds))
--     => Circuit (Apply fs as) (Apply gs bs)
--     -> Circuit (Apply hs cs) (Apply is ds)
--     -> Circuit (Apply (HAppendListR fs hs) (HAppendListR as cs)) (Apply (HAppendListR gs is) (HAppendListR bs ds))
--   Swap      :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Circuit (Apply '[f, g] '[a, b]) (Apply '[g, f] '[b, a])
--   DropL     :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Circuit (Apply '[f, g] '[a, b]) (Apply '[g] '[b])
--   DropR     :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Circuit (Apply '[f, g] '[a, b]) (Apply '[f] '[a])

-- Fixed version
-- data CircuitF iF i o where
--   IdF        :: (DataSource' '[f] '[a] '[f a]) => CircuitF iF (Apply '[f] '[a]) (Apply '[f] '[a])
--   ApplyF     :: (DataSource' fs as (Apply fs as), DataSource' '[g] '[b] '[g b] ) => Task fs as g b -> CircuitF iF (Apply fs as) (Apply '[g] '[b])
--   ReplicateF :: (DataSource' '[f] '[a] '[f a]) => CircuitF iF (Apply '[f] '[a]) (Apply '[f, f] '[a, a])
--   ThenF      :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs))
--     => iF (Apply fs as) (Apply gs bs)
--     -> iF (Apply gs bs) (Apply hs cs)
--     -> CircuitF iF (Apply fs as) (Apply hs cs)
--   BesideF    :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs), DataSource' is ds (Apply is ds))
--     => iF (Apply fs as) (Apply gs bs)
--     -> iF (Apply hs cs) (Apply is ds)
--     -> CircuitF iF (Apply (HAppendListR fs hs) (HAppendListR as cs)) (Apply (HAppendListR gs is) (HAppendListR bs ds))
--   SwapF      :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => CircuitF iF (Apply '[f, g] '[a, b]) (Apply '[g, f] '[b, a])
--   DropLF     :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => CircuitF iF (Apply '[f, g] '[a, b]) (Apply '[g] '[b])
--   DropRF     :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => CircuitF iF (Apply '[f, g] '[a, b]) (Apply '[f] '[a])


-- Fixed modular version
data Id (f :: k -> k -> *) (inputs :: [*]) (outputs :: [*]) where
  Id :: (DataSource' '[inputContainer] '[inputValue] '[inputContainer inputValue]) => Id f '[inputContainer inputValue] '[inputContainer inputVlue]

data Do (iF :: k -> k -> *) (inputs :: [*]) (outputs :: [*]) where
  Do :: (DataSource' fs as (Apply fs as), DataSource' '[g] '[b] '[g b] ) => Task fs as g b -> Do iF (Apply fs as) (Apply '[g] '[b])
  
data Replicate (iF :: k -> k -> *) (inputs :: [*]) (outputs :: [*]) where
  Replicate :: (DataSource' '[f] '[a] '[f a]) => Replicate iF (Apply '[f] '[a]) (Apply '[f, f] '[a, a])

data Then (iF :: k -> k -> *) (inputs :: [*]) (outputs :: [*]) where
  Then :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs))
    => iF (Apply fs as) (Apply gs bs)
    -> iF (Apply gs bs) (Apply hs cs)
    -> Then iF (Apply fs as) (Apply hs cs)

data Beside (iF :: k -> k -> *) (inputs :: [*]) (outputs :: [*]) where
  Beside :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs), DataSource' is ds (Apply is ds))
    => iF (Apply fs as) (Apply gs bs)
    -> iF (Apply hs cs) (Apply is ds)
    -> Beside iF (Apply (HAppendListR fs hs) (HAppendListR as cs)) (Apply (HAppendListR gs is) (HAppendListR bs ds))

data Swap (iF :: k -> k -> *) (inputs :: [*]) (outputs :: [*]) where
  Swap :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => Swap iF (Apply '[f, g] '[a, b]) (Apply '[g, f] '[b, a])

data DropL (iF :: k -> k -> *) (inputs :: [*]) (outputs :: [*]) where
  DropL :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => DropL iF (Apply '[f, g] '[a, b]) (Apply '[g] '[b])

data DropR (iF :: k -> k -> *) (inputs :: [*]) (outputs :: [*]) where
  DropR :: (DataSource' '[f, g] '[a, b] '[f a, g b]) => DropR iF (Apply '[f, g] '[a, b]) (Apply '[f] '[a])


-- IFunctor instances
instance IFunctor2 Id where
  imap2 _ Id = Id

instance IFunctor2 Do where
  imap2 _ (Do t) = Do t

instance IFunctor2 Replicate where
  imap2 _ Replicate = Replicate

instance IFunctor2 Then where
  imap2 f (Then x y) = Then (f x) (f y)

instance IFunctor2 Beside where
  imap2 f (Beside l r) = Beside (f l) (f r)

instance IFunctor2 Swap where
  imap2 _ Swap = Swap

instance IFunctor2 DropL where
  imap2 _ DropL = DropL

instance IFunctor2 DropR where
  imap2 _ DropR = DropR


type CircuitF = Id :+: Do :+: Replicate :+: Then :+: Beside :+: Swap :+: DropL :+: DropR

type Circuit = IFix2 CircuitF


-- Smart Constructors
-- They are able to make use of the `inj` function to add in the L's and R's

id :: (DataSource' '[f] '[a] '[f a], Id :<: iF) => IFix2 iF (Apply '[f] '[a]) (Apply '[f] '[a])
id = (IIn2 . inj) Id

apply :: (DataSource' fs as (Apply fs as), DataSource' '[g] '[b] '[g b], Do :<: iF) => Task fs as g b -> IFix2 iF (Apply fs as) (Apply '[g] '[b])
apply = IIn2 . inj . Do

replicate :: (DataSource' '[f] '[a] '[f a], Replicate :<: iF) => IFix2 iF (Apply '[f] '[a]) (Apply '[f, f] '[a, a])
replicate = (IIn2 . inj) Replicate

(<->) :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs), Then :<: iF)
       => IFix2 iF (Apply fs as) (Apply gs bs)
       -> IFix2 iF (Apply gs bs) (Apply hs cs)
       -> IFix2 iF (Apply fs as) (Apply hs cs)
(<->) l r = IIn2 (inj (Then l r))
infixr 4 <->
  
(<>) :: (DataSource' fs as (Apply fs as), DataSource' gs bs (Apply gs bs), DataSource' hs cs (Apply hs cs), DataSource' is ds (Apply is ds), Beside :<: iF)
       => IFix2 iF (Apply fs as) (Apply gs bs)
       -> IFix2 iF (Apply hs cs) (Apply is ds)
       -> IFix2 iF (Apply (HAppendListR fs hs) (HAppendListR as cs)) (Apply (HAppendListR gs is) (HAppendListR bs ds))
(<>) l r = IIn2 (inj (Beside l r))
infixr 5 <>

swap :: (DataSource' '[f, g] '[a, b] '[f a, g b], Swap :<: iF) => IFix2 iF (Apply '[f, g] '[a, b]) (Apply '[g, f] '[b, a])
swap = (IIn2 . inj) Swap

dropL :: (DataSource' '[f, g] '[a, b] '[f a, g b], DropL :<: iF) => IFix2 iF (Apply '[f, g] '[a, b]) (Apply '[g] '[b])
dropL = (IIn2 . inj) DropL

dropR :: (DataSource' '[f, g] '[a, b] '[f a, g b], DropR :<: iF) => IFix2 iF (Apply '[f, g] '[a, b]) (Apply '[f] '[a])
dropR = (IIn2 . inj) DropR

-- TODO
split n   = undefined

  



