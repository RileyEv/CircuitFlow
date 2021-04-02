{-# LANGUAGE PolyKinds #-}
module Pipeline.Frontend.Circuit where

import Pipeline.Core.DataStore
import Pipeline.Core.Nat 
import Pipeline.Core.Task
import Pipeline.Core.Modular ((:+:)(..), (:<:)(..))
import Pipeline.Core.IFunctor (IFix6(..), IFunctor6(..))
import Pipeline.Backend.ProcessNetwork (AppendP)
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
data Id (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> *)
        (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
        (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) where
  Id :: (DataSource' '[inputS] '[inputT]) => Id iF '[inputS] '[inputT] '[inputS inputT] '[inputS] '[inputT] '[inputS inputT]

data Replicate (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> *)
               (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
               (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) where
  Replicate :: (DataSource' '[f] '[a]) => Replicate iF '[f] '[a] '[f a] '[f, f] '[a, a] '[f a, f a]

data Then (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> *)
          (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
          (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) where
  Then :: (DataSource' fs as, DataSource' gs bs, DataSource' hs cs)
    => iF fs as (Apply fs as) gs bs (Apply gs bs)
    -> iF gs bs (Apply gs bs) hs cs (Apply hs cs)
    -> Then iF fs as (Apply fs as) hs cs (Apply hs cs)

data Beside (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> *)
            (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
            (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) where
  Beside :: (DataSource' fs as,
             DataSource' gs bs,
             DataSource' hs cs,
             DataSource' is ds,
             Length fs ~ Length as,
             Length fs ~ Length (Apply fs as),
             Length gs ~ Length bs,
             Length gs ~ Length (Apply gs bs),
             Length hs ~ Length cs,
             Length hs ~ Length (Apply hs cs),
             Length is ~ Length ds,
             Length is ~ Length (Apply is ds),
             Take (Length as) (Apply fs as :++ Apply hs cs) ~ Apply fs as,
             Take (Length as) (as :++ cs) ~ as,
             Take (Length as) (fs :++ hs) ~ fs,
             Drop (Length as) (Apply fs as :++ Apply hs cs) ~ Apply hs cs,
             Drop (Length as) (as :++ cs) ~ cs,
             Drop (Length as) (fs :++ hs) ~ hs,
             AppendP gs bs (Apply gs bs) is ds (Apply is ds)
             )
    => iF fs as (Apply fs as) gs bs (Apply gs bs)
    -> iF hs cs (Apply hs cs) is ds (Apply is ds)
    -> Beside iF (fs :++ hs) (as :++ cs) (Apply fs as :++ Apply hs cs) (gs :++ is) (bs :++ ds) (Apply gs bs :++ Apply is ds)

data Swap (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> *)
          (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
          (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) where
  Swap :: (DataSource' '[f, g] '[a, b]) => Swap iF '[f, g] '[a, b] '[f a, g b] '[g, f] '[b, a] '[g b, f a]

data DropL (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> *)
           (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
           (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) where
  DropL :: (DataSource' '[f, g] '[a, b]) => DropL iF '[f, g] '[a, b] '[f a, g b] '[g] '[b] '[g b]

data DropR (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> *)
           (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
           (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) where
  DropR :: (DataSource' '[f, g] '[a, b]) => DropR iF '[f, g] '[a, b] '[f a, g b] '[f] '[a] '[f a]


-- IFunctor instances
instance IFunctor6 Id where
  imap6 _ Id = Id

instance IFunctor6 Replicate where
  imap6 _ Replicate = Replicate

instance IFunctor6 Then where
  imap6 f (Then x y) = Then (f x) (f y)

instance IFunctor6 Beside where
  imap6 f (Beside l r) = Beside (f l) (f r)

instance IFunctor6 Swap where
  imap6 _ Swap = Swap

instance IFunctor6 DropL where
  imap6 _ DropL = DropL

instance IFunctor6 DropR where
  imap6 _ DropR = DropR


type CircuitF v = Id :+: Replicate :+: Then :+: Beside :+: Swap :+: DropL :+: DropR :+: v

type Circuit' v = IFix6 (CircuitF v)

type Circuit = Circuit' TaskF

-- Smart Constructors
-- They are able to make use of the `inj` function to add in the L's and R's

id :: (DataSource' '[f] '[a], Id :<: iF) => IFix6 iF '[f] '[a] '[f a] '[f] '[a] '[f a]
id = (IIn6 . inj) Id

replicate :: (DataSource' '[f] '[a], Replicate :<: iF) => IFix6 iF '[f] '[a] '[f a] '[f, f] '[a, a] '[f a, f a]
replicate = (IIn6 . inj) Replicate

(<->) :: (DataSource' fs as, DataSource' gs bs, DataSource' hs cs, Then :<: iF)
       => IFix6 iF fs as (Apply fs as) gs bs (Apply gs bs)
       -> IFix6 iF gs bs (Apply gs bs) hs cs (Apply hs cs)
       -> IFix6 iF fs as (Apply fs as) hs cs (Apply hs cs)
(<->) l r = IIn6 (inj (Then l r))
infixr 4 <->
  
(<>) :: (DataSource' fs as,
         DataSource' gs bs,
         DataSource' hs cs,
         DataSource' is ds,
         Length fs ~ Length as,
         Length fs ~ Length (Apply fs as),
         Length gs ~ Length bs,
         Length gs ~ Length (Apply gs bs),
         Length hs ~ Length cs,
         Length hs ~ Length (Apply hs cs),
         Length is ~ Length ds,
         Length is ~ Length (Apply is ds),
         Take (Length as) (Apply fs as :++ Apply hs cs) ~ Apply fs as,
         Take (Length as) (as :++ cs) ~ as,
         Take (Length as) (fs :++ hs) ~ fs,
         Drop (Length as) (Apply fs as :++ Apply hs cs) ~ Apply hs cs,
         Drop (Length as) (as :++ cs) ~ cs,
         Drop (Length as) (fs :++ hs) ~ hs,
         AppendP gs bs (Apply gs bs) is ds (Apply is ds),
         Beside :<: iF)
       => IFix6 iF fs as (Apply fs as) gs bs (Apply gs bs)
       -> IFix6 iF hs cs (Apply hs cs) is ds (Apply is ds)
       -> IFix6 iF (fs :++ hs) (as :++ cs) (Apply fs as :++ Apply hs cs) (gs :++ is) (bs :++ ds) (Apply gs bs :++ Apply is ds)
(<>) l r = IIn6 (inj (Beside l r))
infixr 5 <>

swap :: (DataSource' '[f, g] '[a, b], Swap :<: iF) => IFix6 iF '[f, g] '[a, b] '[f a, g b] '[g, f] '[b, a] '[g b, f a]
swap = (IIn6 . inj) Swap

dropL :: (DataSource' '[f, g] '[a, b], DropL :<: iF) => IFix6 iF '[f, g] '[a, b] '[f a, g b] '[g] '[b] '[g b]
dropL = (IIn6 . inj) DropL

dropR :: (DataSource' '[f, g] '[a, b], DropR :<: iF) => IFix6 iF '[f, g] '[a, b] '[f a, g b] '[f] '[a] '[f a]
dropR = (IIn6 . inj) DropR




-- TODO
-- replicate' :: (Replicate :<: iF) => SNat n -> IFix4 iF input (ReplicateN n input)
-- replicate' _   = undefined

  



