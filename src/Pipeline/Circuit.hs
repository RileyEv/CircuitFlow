module Pipeline.Circuit (
  Circuit,
  id,
  replicate,
  (<->),
  (<>),
  swap,
  dropL,
  dropR
) where  


import Pipeline.Internal.Core.CircuitAST
import Pipeline.Internal.Core.DataStore (DataStore')
import Pipeline.Internal.Core.PipeList (AppendP)
import Pipeline.Internal.Common.IFunctor (IFix7(..))
import Pipeline.Internal.Common.IFunctor.Modular ((:<:)(..))
import Pipeline.Internal.Common.Nat (Nat(..), IsNat, (:+))
import Pipeline.Internal.Common.TypeList (Length, Drop, Take, Apply, (:++))

import Prelude hiding (id, replicate, (<>))

id :: (DataStore' '[f] '[a], Id :<: iF)
  => IFix7 iF '[f] '[a] '[f a] '[f] '[a] '[f a] ('Succ 'Zero)
id = (IIn7 . inj) Id

replicate :: (DataStore' '[f] '[a], Replicate :<: iF)
  => IFix7 iF '[f] '[a] '[f a] '[f, f] '[a, a] '[f a, f a] ('Succ 'Zero)
replicate = (IIn7 . inj) Replicate

(<->) :: (DataStore' fs as, DataStore' gs bs, DataStore' hs cs, Then :<: iF)
       => IFix7 iF fs as (Apply fs as) gs bs (Apply gs bs) nfs
       -> IFix7 iF gs bs (Apply gs bs) hs cs (Apply hs cs) ngs
       -> IFix7 iF fs as (Apply fs as) hs cs (Apply hs cs) nfs
(<->) l r = IIn7 (inj (Then l r))
infixr 4 <->
  
(<>) :: (DataStore' fs as,
         DataStore' gs bs,
         DataStore' hs cs,
         DataStore' is ds,
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
       -> IFix7 iF (fs :++ hs) (as :++ cs) (Apply fs as :++ Apply hs cs)
                   (gs :++ is) (bs :++ ds) (Apply gs bs :++ Apply is ds) (nfs :+ nhs)
(<>) l r = IIn7 (inj (Beside l r))
infixr 5 <>

swap :: (DataStore' '[f, g] '[a, b], Swap :<: iF)
  => IFix7 iF '[f, g] '[a, b] '[f a, g b] '[g, f] '[b, a] '[g b, f a] ('Succ ('Succ 'Zero))
swap = (IIn7 . inj) Swap

dropL :: (DataStore' '[f, g] '[a, b], DropL :<: iF)
  => IFix7 iF '[f, g] '[a, b] '[f a, g b] '[g] '[b] '[g b] ('Succ ('Succ 'Zero))
dropL = (IIn7 . inj) DropL

dropR :: (DataStore' '[f, g] '[a, b], DropR :<: iF)
  => IFix7 iF '[f, g] '[a, b] '[f a, g b] '[f] '[a] '[f a] ('Succ ('Succ 'Zero))
dropR = (IIn7 . inj) DropR
