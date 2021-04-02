{-# LANGUAGE PolyKinds #-}
module Pipeline.Frontend.Circuit where

import Pipeline.Core.DataStore
import Pipeline.Core.Nat 
import Pipeline.Core.Task
import Pipeline.Core.Modular ((:+:)(..), (:<:)(..))
import Pipeline.Core.IFunctor (IFix7(..), IFunctor7(..))
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
data Id (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> Nat -> *)
        (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
        (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) (ninputs :: Nat) where
  Id :: (DataSource' '[inputS] '[inputT]) => Id iF '[inputS] '[inputT] '[inputS inputT] '[inputS] '[inputT] '[inputS inputT] ('Succ 'Zero)

data Replicate (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> Nat -> *)
               (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
               (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) (ninputs :: Nat) where
  Replicate :: (DataSource' '[f] '[a]) => Replicate iF '[f] '[a] '[f a] '[f, f] '[a, a] '[f a, f a] ('Succ 'Zero)

data Then (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> Nat -> *)
          (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
          (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) (ninputs :: Nat) where
  Then :: (DataSource' fs as, DataSource' gs bs, DataSource' hs cs)
    => iF fs as (Apply fs as) gs bs (Apply gs bs) nfs
    -> iF gs bs (Apply gs bs) hs cs (Apply hs cs) ngs
    -> Then iF fs as (Apply fs as) hs cs (Apply hs cs) nfs

data Beside (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> Nat -> *)
            (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
            (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) (ninputs :: Nat) where
  Beside :: (DataSource' fs as,
             DataSource' gs bs,
             DataSource' hs cs,
             DataSource' is ds,
             IsNat nfs,
             IsNat nhs,
             nfs ~ Length fs,
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
    => iF fs as (Apply fs as) gs bs (Apply gs bs) nfs
    -> iF hs cs (Apply hs cs) is ds (Apply is ds) nhs
    -> Beside iF (fs :++ hs) (as :++ cs) (Apply fs as :++ Apply hs cs) (gs :++ is) (bs :++ ds) (Apply gs bs :++ Apply is ds) (nfs + nhs)

data Swap (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> Nat -> *)
          (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
          (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) (ninputs :: Nat) where
  Swap :: (DataSource' '[f, g] '[a, b]) => Swap iF '[f, g] '[a, b] '[f a, g b] '[g, f] '[b, a] '[g b, f a] ('Succ ('Succ 'Zero))

data DropL (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> Nat -> *)
           (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
           (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) (ninputs :: Nat) where
  DropL :: (DataSource' '[f, g] '[a, b]) => DropL iF '[f, g] '[a, b] '[f a, g b] '[g] '[b] '[g b] ('Succ ('Succ 'Zero))

data DropR (iF :: [* -> *] -> [*] -> [*] -> [* -> *] -> [*] -> [*] -> Nat -> *)
           (inputsS :: [* -> *]) (inputsT :: [*]) (inputsA :: [*])
           (outputsS :: [* -> *]) (outputsT :: [*]) (outputsA :: [*]) (ninputs :: Nat) where
  DropR :: (DataSource' '[f, g] '[a, b]) => DropR iF '[f, g] '[a, b] '[f a, g b] '[f] '[a] '[f a] ('Succ ('Succ 'Zero))


-- IFunctor instances
instance IFunctor7 Id where
  imap7 _ Id = Id

instance IFunctor7 Replicate where
  imap7 _ Replicate = Replicate

instance IFunctor7 Then where
  imap7 f (Then x y) = Then (f x) (f y)

instance IFunctor7 Beside where
  imap7 f (Beside l r) = Beside (f l) (f r)

instance IFunctor7 Swap where
  imap7 _ Swap = Swap

instance IFunctor7 DropL where
  imap7 _ DropL = DropL

instance IFunctor7 DropR where
  imap7 _ DropR = DropR


type CircuitF v = Id :+: Replicate :+: Then :+: Beside :+: Swap :+: DropL :+: DropR :+: v

type Circuit' v = IFix7 (CircuitF v)

type Circuit = Circuit' TaskF

-- Smart Constructors
-- They are able to make use of the `inj` function to add in the L's and R's

id :: (DataSource' '[f] '[a], Id :<: iF) => IFix7 iF '[f] '[a] '[f a] '[f] '[a] '[f a] ('Succ 'Zero)
id = (IIn7 . inj) Id

replicate :: (DataSource' '[f] '[a], Replicate :<: iF) => IFix7 iF '[f] '[a] '[f a] '[f, f] '[a, a] '[f a, f a] ('Succ 'Zero)
replicate = (IIn7 . inj) Replicate

(<->) :: (DataSource' fs as, DataSource' gs bs, DataSource' hs cs, Then :<: iF)
       => IFix7 iF fs as (Apply fs as) gs bs (Apply gs bs) nfs
       -> IFix7 iF gs bs (Apply gs bs) hs cs (Apply hs cs) ngs
       -> IFix7 iF fs as (Apply fs as) hs cs (Apply hs cs) nfs
(<->) l r = IIn7 (inj (Then l r))
infixr 4 <->
  
(<>) :: (DataSource' fs as,
         DataSource' gs bs,
         DataSource' hs cs,
         DataSource' is ds,
         nfs ~ Length fs,
         IsNat nfs,
         IsNat nhs,
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
       => IFix7 iF fs as (Apply fs as) gs bs (Apply gs bs) nfs
       -> IFix7 iF hs cs (Apply hs cs) is ds (Apply is ds) nhs
       -> IFix7 iF (fs :++ hs) (as :++ cs) (Apply fs as :++ Apply hs cs) (gs :++ is) (bs :++ ds) (Apply gs bs :++ Apply is ds) (nfs + nhs)
(<>) l r = IIn7 (inj (Beside l r))
infixr 5 <>

swap :: (DataSource' '[f, g] '[a, b], Swap :<: iF) => IFix7 iF '[f, g] '[a, b] '[f a, g b] '[g, f] '[b, a] '[g b, f a] ('Succ ('Succ 'Zero))
swap = (IIn7 . inj) Swap

dropL :: (DataSource' '[f, g] '[a, b], DropL :<: iF) => IFix7 iF '[f, g] '[a, b] '[f a, g b] '[g] '[b] '[g b] ('Succ ('Succ 'Zero))
dropL = (IIn7 . inj) DropL

dropR :: (DataSource' '[f, g] '[a, b], DropR :<: iF) => IFix7 iF '[f, g] '[a, b] '[f a, g b] '[f] '[a] '[f a] ('Succ ('Succ 'Zero))
dropR = (IIn7 . inj) DropR




-- TODO
-- replicate' :: (Replicate :<: iF) => SNat n -> IFix4 iF input (ReplicateN n input)
-- replicate' _   = undefined

  



