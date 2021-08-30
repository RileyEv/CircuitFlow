{-|
Module      : Pipeline.Circuit
Description : Constructors needed to build Circuits
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

This package contains all the constructors needed to build a 'Circuit'.

-}
module Pipeline.Circuit
  (
  -- * Main Type
    AST.Circuit
  ,
  -- * Constructors
    id
  , replicate2
  , replicateN
  , (<->)
  , (<>)
  , swap
  , dropL
  , dropR
  , mapC
  ) where


import           Pipeline.Internal.Common.IFunctor         (IFix5 (..))
import           Pipeline.Internal.Common.IFunctor.Modular ((:<:) (..))
import           Pipeline.Internal.Common.Nat              (IsNat, N1, N2,
                                                            Nat (..), SNat (..), (:+))
import           Pipeline.Internal.Common.TypeList         (Apply, Drop, Length,
                                                            Take, Replicate, (:++))
import qualified Pipeline.Internal.Core.CircuitAST as AST
import           Pipeline.Internal.Core.DataStore          (DataStore,
                                                            DataStore', Var)
import           Pipeline.Internal.Core.PipeList           (AppendP)

import           Prelude                                   hiding (id,
                                                            replicate, (<>))


{-|
Passes an input through without modifying it.

In diagram form it would look like,

> |

-}
id :: (DataStore' '[f] '[a]) => AST.Circuit '[f] '[a] '[f] '[a] N1
id = (IIn5 . inj) AST.Id

{-|
Duplicates an input.

In diagram form it would look like,

> /\

-}
replicate2 :: DataStore' '[f] '[a] => AST.Circuit '[f] '[a] '[f , f] '[a , a] N1
replicate2 = (IIn5 . inj) AST.Replicate

{-|
Usually referred to as \"then\", this operator joins two levels of a circuit together.

A diagram representing @a \<-\> b@ or \"a then b\" can be seen below,

> | ... |    -- Any number of inputs
>    a
> | ... |    -- outputs a ~ inputs b
>    b
> | ... |    -- Any number of outputs

-}
(<->)
  :: (DataStore' fs as, DataStore' gs bs, DataStore' hs cs)
  => AST.Circuit fs as gs bs nfs -- ^ First circuit (@a@)
  -> AST.Circuit gs bs hs cs ngs -- ^ Second circuit (@b@)
  -> AST.Circuit fs as hs cs nfs
(<->) l r = IIn5 (inj (AST.Then l r))
infixr 4 <->


{-|
Usually referred to as \"beside\" or \"next to\", this operator joins two circuits next to each other.

A diagram representing @a \<\> b@ or \"a next to b\" can be seen below,

> | ... | ... |  -- inputs  a ++ inputs  b
>    a     b
> | ... | ... |  -- outputs a ++ outputs b

-}
(<>)
  :: ( DataStore' fs as
     , DataStore' gs bs
     , DataStore' hs cs
     , DataStore' is ds
     , nfs ~ Length fs
     , IsNat nfs
     , IsNat nhs
     , Length fs ~ Length as
     , Length gs ~ Length bs
     , Length hs ~ Length cs
     , Length is ~ Length ds
     , Take (Length as) (as :++ cs) ~ as
     , Take (Length as) (fs :++ hs) ~ fs
     , Drop (Length as) (as :++ cs) ~ cs
     , Drop (Length as) (fs :++ hs) ~ hs
     , AppendP gs bs is ds
     )
  => AST.Circuit fs as gs bs nfs -- ^ Left circuit
  -> AST.Circuit hs cs is ds nhs -- ^ Right circuit
  -> AST.Circuit
       (fs :++ hs)
       (as :++ cs)
       (gs :++ is)
       (bs :++ ds)
       (nfs :+ nhs)
(<>) l r = IIn5 (inj (AST.Beside l r))
infixr 5 <>

{-|
Swaps to input values around.

In diagram form this would look like,

> \/
> /\

-}
swap
  :: (DataStore' '[f , g] '[a , b])
  => AST.Circuit '[f , g] '[a , b] '[g , f] '[b , a] N2
swap = (IIn5 . inj) AST.Swap

{-|
Takes two values as input and drops the left input.
-}
dropL
  :: (DataStore' '[f , g] '[a , b]) => AST.Circuit '[f , g] '[a , b] '[g] '[b] N2
dropL = (IIn5 . inj) AST.DropL

{-|
Takes two values as input and drops the right input.
-}
dropR
  :: (DataStore' '[f , g] '[a , b]) => AST.Circuit '[f , g] '[a , b] '[f] '[a] N2
dropR = (IIn5 . inj) AST.DropR

{-|
Maps a circuit on the inputs
-}
mapC
  :: (DataStore' '[f] '[[a]], DataStore g [b], Eq (g [b]), Eq a)
  => AST.Circuit '[Var] '[a] '[Var] '[b] N1
  -> AST.Circuit '[f] '[[a]] '[g] '[[b]] N1
mapC c = (IIn5 . inj) (AST.Map c)

class (DataStore f a, Eq (f a)) => ReplicateN n f a where
  replicateN :: SNat n -> AST.Circuit '[f] '[a] (Replicate n f) (Replicate n a)  N1

instance (DataStore f a, Eq a, Eq (f a)) => ReplicateN ('Succ ('Succ 'Zero)) f a where
  replicateN (SSucc (SSucc SZero)) = replicate2

instance (DataStore f a, Eq a, Eq (f a)) => ReplicateN ('Succ ('Succ ('Succ 'Zero))) f a where
  replicateN (SSucc n) = replicate2 <-> id <> replicateN n

replicateMany :: SNat m -> AST.Circuit fs as (fs :++ fs) (as :++ as) m
replicateMany = undefined
