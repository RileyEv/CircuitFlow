{-|
Module      : Pipeline.Circuit
Description : Constructors needed to build Circuits
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

This package contains all the constructors needed to build a 'Circuit'.

-}
module Pipeline.Circuit (
  -- * Main Type
  Circuit,
  -- * Constructors
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
import Pipeline.Internal.Common.Nat (Nat(..), IsNat, (:+), N1, N2)
import Pipeline.Internal.Common.TypeList (Length, Drop, Take, Apply, (:++))

import Prelude hiding (id, replicate, (<>))


{-|
Passes an input through without modifying it.

In diagram form it would look like, 

> |

-}
id :: (DataStore' '[f] '[a])
  => Circuit '[f] '[a] '[f a] '[f] '[a] '[f a] N1
id = (IIn7 . inj) Id

{-|
Duplicates an input.

In diagram form it would look like,

> /\

-}
replicate :: (DataStore' '[f] '[a])
  => Circuit '[f] '[a] '[f a] '[f, f] '[a, a] '[f a, f a] N1
replicate = (IIn7 . inj) Replicate

{-|
Usually referred to as \"then\", this operator joins two levels of a circuit together.

A diagram representing @a \<-\> b@ or \"a then b\" can be seen below,

> | ... |    -- Any number of inputs
>    a
> | ... |    -- outputs a ~ inputs b
>    b
> | ... |    -- Any number of outputs

-}
(<->) :: (DataStore' fs as, DataStore' gs bs, DataStore' hs cs)
       => Circuit fs as (Apply fs as) gs bs (Apply gs bs) nfs -- ^ First circuit (@a@)
       -> Circuit gs bs (Apply gs bs) hs cs (Apply hs cs) ngs -- ^ Second circuit (@b@)
       -> Circuit fs as (Apply fs as) hs cs (Apply hs cs) nfs
(<->) l r = IIn7 (inj (Then l r))
infixr 4 <->


{-|
Usually referred to as \"beside\" or \"next to\", this operator joins two circuits next to each other.

A diagram representing @a \<\> b@ or \"a next to b\" can be seen below,

> | ... | ... |  -- inputs  a ++ inputs  b
>    a     b
> | ... | ... |  -- outputs a ++ outputs b

-}
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
         AppendP gs bs (Apply gs bs) is ds (Apply is ds))
       => Circuit fs as (Apply fs as) gs bs (Apply gs bs) nfs -- ^ Left circuit
       -> Circuit hs cs (Apply hs cs) is ds (Apply is ds) nhs -- ^ Right circuit
       -> Circuit (fs :++ hs) (as :++ cs) (Apply fs as :++ Apply hs cs)
                   (gs :++ is) (bs :++ ds) (Apply gs bs :++ Apply is ds) (nfs :+ nhs)
(<>) l r = IIn7 (inj (Beside l r))
infixr 5 <>

{-|
Swaps to input values around.

In diagram form this would look like,

> \/
> /\

-}
swap :: (DataStore' '[f, g] '[a, b])
  => Circuit '[f, g] '[a, b] '[f a, g b] '[g, f] '[b, a] '[g b, f a] N2
swap = (IIn7 . inj) Swap

{-|
Takes two values as input and drops the left input.
-}
dropL :: (DataStore' '[f, g] '[a, b])
  => Circuit '[f, g] '[a, b] '[f a, g b] '[g] '[b] '[g b] N2
dropL = (IIn7 . inj) DropL

{-|
Takes two values as input and drops the right input.
-}
dropR :: (DataStore' '[f, g] '[a, b])
  => Circuit '[f, g] '[a, b] '[f a, g b] '[f] '[a] '[f a] N2
dropR = (IIn7 . inj) DropR
