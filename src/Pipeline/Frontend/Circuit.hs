{-# LANGUAGE PolyKinds #-}
module Pipeline.Frontend.Circuit where

import Pipeline.Core.DataStore
import Pipeline.Core.Task
import Pipeline.Core.Modular ((:+:)(..), (:<:)(..))
import Pipeline.Core.IFunctor (IFix4(..), IFunctor4(..))
-- import Pipeline.Core.Nat (Take, Drop, Length)
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
data Id (f :: [* -> *] -> [*] -> [* -> *] -> [*] -> *) (inputsS :: [* -> *]) (inputsT :: [*]) (outputsS :: [* -> *]) (outputs :: [*]) where
  Id :: (DataSource' '[inputS] '[inputT]) => Id f '[inputS] '[inputT] '[inputS] '[inputT]

-- data Do (iF :: k -> k -> *) (inputs :: [*]) (outputs :: [*]) where
--   Do :: (DataSource' fs as (Apply fs as), DataSource' '[g] '[b] '[g b] ) => Task fs as g b -> Do iF (Apply fs as) (Apply '[g] '[b])
  
data Replicate (iF :: [* -> *] -> [*] -> [* -> *] -> [*] -> *) (inputsS :: [* -> *]) (inputsT :: [*]) (outputsS :: [* -> *]) (outputsT :: [*]) where
  Replicate :: (DataSource' '[f] '[a]) => Replicate iF '[f] '[a] '[f, f] '[a, a]

data Then (iF :: [* -> *] -> [*] -> [* -> *] -> [*] -> *) (inputsS :: [* -> *]) (inputsT :: [*]) (outputsS :: [* -> *]) (outputs :: [*]) where
  Then :: (DataSource' fs as, DataSource' gs bs, DataSource' hs cs)
    => iF fs as gs bs
    -> iF gs bs hs cs
    -> Then iF fs as hs cs

data Beside (iF :: [* -> *] -> [*] -> [* -> *] -> [*] -> *) (inputsS :: [* -> *]) (inputsT :: [*]) (outputsS :: [* -> *]) (outputsT :: [*]) where
  Beside :: (DataSource' fs as,
             DataSource' gs bs,
             DataSource' hs cs,
             DataSource' is ds)
    => iF fs as gs bs
    -> iF hs cs is ds
    -> Beside iF (fs ++ hs) (as ++ cs) (gs ++ is) (bs ++ ds)

data Swap (iF :: [* -> *] -> [*] -> [* -> *] -> [*] -> *) (inputsS :: [* -> *]) (inputsT :: [*]) (outputsS :: [* -> *]) (outputsT :: [*]) where
  Swap :: (DataSource' '[f, g] '[a, b]) => Swap iF '[f, g] '[a, b] '[g, f] '[b, a]

data DropL (iF :: [* -> *] -> [*] -> [* -> *] -> [*] -> *) (inputsS :: [* -> *]) (inputsT :: [*]) (outputsS :: [* -> *]) (outputsT :: [*]) where
  DropL :: (DataSource' '[f, g] '[a, b]) => DropL iF '[f, g] '[a, b] '[g] '[b]

data DropR (iF :: [* -> *] -> [*] -> [* -> *] -> [*] -> *) (inputsS :: [* -> *]) (inputsT :: [*]) (outputsS :: [* -> *]) (outputsT :: [*]) where
  DropR :: (DataSource' '[f, g] '[a, b]) => DropR iF '[f, g] '[a, b] '[f] '[a]


-- IFunctor instances
instance IFunctor4 Id where
  imap4 _ Id = Id

-- instance IFunctor4 Do where
--   imap4 _ (Do t) = Do t

instance IFunctor4 Replicate where
  imap4 _ Replicate = Replicate

instance IFunctor4 Then where
  imap4 f (Then x y) = Then (f x) (f y)

instance IFunctor4 Beside where
  imap4 f (Beside l r) = Beside (f l) (f r)

instance IFunctor4 Swap where
  imap4 _ Swap = Swap

instance IFunctor4 DropL where
  imap4 _ DropL = DropL

instance IFunctor4 DropR where
  imap4 _ DropR = DropR


type CircuitF v = Id :+: Replicate :+: Then :+: Beside :+: Swap :+: DropL :+: DropR :+: v

type Circuit' v = IFix4 (CircuitF v)

type Circuit = Circuit' TaskF

-- Smart Constructors
-- They are able to make use of the `inj` function to add in the L's and R's

id :: (DataSource' '[f] '[a], Id :<: iF) => IFix4 iF '[f] '[a] '[f] '[a]
id = (IIn4 . inj) Id

-- apply :: (DataSource' fs as (Apply fs as), DataSource' '[g] '[b] '[g b], Do :<: iF) => Task fs as g b -> IFix4 iF (Apply fs as) (Apply '[g] '[b])
-- apply = IIn4 . inj . Do

replicate :: (DataSource' '[f] '[a], Replicate :<: iF) => IFix4 iF '[f] '[a] '[f, f] '[a, a]
replicate = (IIn4 . inj) Replicate

(<->) :: (DataSource' fs as, DataSource' gs bs, DataSource' hs cs, Then :<: iF)
       => IFix4 iF fs as gs bs
       -> IFix4 iF gs bs hs cs
       -> IFix4 iF fs as hs cs
(<->) l r = IIn4 (inj (Then l r))
infixr 4 <->
  
(<>) :: (DataSource' fs as,
         DataSource' gs bs,
         DataSource' hs cs,
         DataSource' is ds,
         Beside :<: iF)
       => IFix4 iF fs as gs bs
       -> IFix4 iF hs cs is ds
       -> IFix4 iF (fs ++ hs) (as ++ cs) (gs ++ is) (bs ++ ds)
(<>) l r = IIn4 (inj (Beside l r))
infixr 5 <>

swap :: (DataSource' '[f, g] '[a, b], Swap :<: iF) => IFix4 iF '[f, g] '[a, b] '[g, f] '[b, a]
swap = (IIn4 . inj) Swap

dropL :: (DataSource' '[f, g] '[a, b], DropL :<: iF) => IFix4 iF '[f, g] '[a, b] '[g] '[b]
dropL = (IIn4 . inj) DropL

dropR :: (DataSource' '[f, g] '[a, b], DropR :<: iF) => IFix4 iF '[f, g] '[a, b] '[f] '[a]
dropR = (IIn4 . inj) DropR




-- TODO
-- replicate' :: (Replicate :<: iF) => SNat n -> IFix4 iF input (ReplicateN n input)
-- replicate' _   = undefined

  



