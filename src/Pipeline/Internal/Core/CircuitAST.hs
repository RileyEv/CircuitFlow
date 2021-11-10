module Pipeline.Internal.Core.CircuitAST
  ( Circuit
  , CircuitF
  ,
  -- * Constructors
    Id(..)
  , Replicate(..)
  , Then(..)
  , Beside(..)
  , Swap(..)
  , DropL(..)
  , DropR(..)
  , Task(..)
  , Map(..)
  ) where

import           Control.Exception                         (SomeException)
import           Control.Monad.Except                      (ExceptT)
import           Data.Kind                                 (Type)
import           Pipeline.Internal.Common.HList            (HList' (..))
import           Pipeline.Internal.Common.IFunctor         (IFix5 (..),
                                                            IFunctor5 (..))
import           Pipeline.Internal.Common.IFunctor.Modular ((:+:) (..))
import           Pipeline.Internal.Common.Nat              (IsNat (..), N1, N2,
                                                            Nat (..), (:+),
                                                            (:=), SNat (..))
import           Pipeline.Internal.Common.TypeList         (Drop, Length,
                                                            Take, (:++))
import           Pipeline.Internal.Core.DataStore          (DataStore,
                                                            DataStore', Var)
import           Pipeline.Internal.Core.PipeList           (AppendP)

data Id (iF :: [Type -> Type] -> [Type] -> [Type -> Type] -> [Type] -> Nat -> Type)
        (inputsS  :: [Type -> Type]) (inputsT  :: [Type])
        (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  Id ::(DataStore' '[inputS] '[inputT])
    => Id iF '[inputS] '[inputT] '[inputS] '[inputT] N1

data Replicate (iF :: [Type -> Type] -> [Type] -> [Type -> Type] -> [Type] -> Nat -> Type)
               (inputsS :: [Type -> Type]) (inputsT :: [Type])
               (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  Replicate ::(DataStore' '[f] '[a])
    => Replicate iF '[f] '[a] [f, f] '[a, a] N1

data ReplicateManyN (iF :: [Type -> Type] -> [Type] -> [Type -> Type] -> [Type] -> Nat -> Type)
               (inputsS :: [Type -> Type]) (inputsT :: [Type])
               (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  ReplicateManyN ::(DataStore' '[f] '[a])
    => SNat n -> SNat m -> ReplicateManyN iF '[f] '[a] [f, f] '[a, a] N1


data Then (iF :: [Type -> Type] -> [Type] -> [Type -> Type] -> [Type] -> Nat -> Type)
          (inputsS :: [Type -> Type]) (inputsT :: [Type])
          (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  Then ::(DataStore' fs as, DataStore' gs bs, DataStore' hs cs)
    => iF fs as gs bs nfs
    -> iF gs bs hs cs ngs
    -> Then iF fs as hs cs nfs

data Beside (iF :: [Type -> Type] -> [Type] -> [Type -> Type] -> [Type] -> Nat -> Type)
            (inputsS :: [Type -> Type]) (inputsT :: [Type])
            (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  Beside ::(DataStore' fs as,
             DataStore' gs bs,
             DataStore' hs cs,
             DataStore' is ds,
             IsNat nfs,
             IsNat nhs,
             nfs ~ Length fs,
             Length fs ~ Length as,
             Length gs ~ Length bs,
             Length hs ~ Length cs,
             Length is ~ Length ds,
             Take (Length as) (as :++ cs) ~ as,
             Take (Length as) (fs :++ hs) ~ fs,
             Drop (Length as) (as :++ cs) ~ cs,
             Drop (Length as) (fs :++ hs) ~ hs,
             AppendP gs bs is ds
             )
    => iF fs as gs bs nfs
    -> iF hs cs is ds nhs
    -> Beside iF (fs :++ hs) (as :++ cs)
                 (gs :++ is) (bs :++ ds) (nfs :+ nhs)

data Swap (iF :: [Type -> Type] -> [Type] -> [Type -> Type] -> [Type] -> Nat -> Type)
          (inputsS :: [Type -> Type]) (inputsT :: [Type])
          (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  Swap ::(DataStore' '[f, g] '[a, b])
    => Swap iF '[f, g] '[a, b] '[g, f] '[b, a] N2

data DropL (iF :: [Type -> Type] -> [Type] -> [Type -> Type] -> [Type] -> Nat -> Type)
           (inputsS :: [Type -> Type]) (inputsT :: [Type])
           (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  DropL ::(DataStore' '[f, g] '[a, b])
    => DropL iF '[f, g] '[a, b] '[g] '[b] N2

data DropR (iF :: [Type -> Type] -> [Type] -> [Type -> Type]-> [Type] -> Nat -> Type)
           (inputsS :: [Type -> Type]) (inputsT :: [Type])
           (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  DropR ::(DataStore' '[f, g] '[a, b])
    => DropR iF '[f, g] '[a, b] '[f] '[a] N2


data Task (iF :: [Type -> Type] -> [Type] -> [Type -> Type] -> [Type] -> Nat -> Type)
           (inputsS :: [Type -> Type]) (inputsT :: [Type])
           (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  Task ::(Length outputsS := 'Succ 'Zero ~ 'True,
           outputsS ~ '[g'], outputsT ~ '[b'],
           DataStore' inputsS inputsT,
           DataStore g' b', Eq (g' b'))
       => (HList' inputsS inputsT -> g' b' -> ExceptT SomeException IO ())
       -> Task iF inputsS inputsT outputsS outputsT (Length inputsS)


data Map (iF :: [Type -> Type] -> [Type] -> [Type -> Type] -> [Type] -> Nat -> Type)
         (inputsS :: [Type -> Type]) (inputsT :: [Type])
         (outputsS :: [Type -> Type]) (outputsT :: [Type]) (ninputs :: Nat) where
  Map ::(DataStore' '[f] '[[a]], DataStore g [b], Eq (g [b]), Eq a)
    => Circuit '[Var] '[a] '[Var] '[b] N1
    -> Map iF '[f] '[[a]] '[g] '[[b]] N1


-- IFunctor instances
instance IFunctor5 Id where
  imap5 _ Id = Id
  imapM5 _ Id = return Id

instance IFunctor5 Replicate where
  imap5 _ Replicate = Replicate
  imapM5 _ Replicate = return Replicate

instance IFunctor5 Then where
  imap5 f (Then x y) = Then (f x) (f y)
  imapM5 f (Then x y) = do
    x' <- f x
    y' <- f y
    return (Then x' y')

instance IFunctor5 Beside where
  imap5 f (Beside l r) = Beside (f l) (f r)
  imapM5 f (Beside l r) = do
    l' <- f l
    r' <- f r
    return (Beside l' r')

instance IFunctor5 Swap where
  imap5 _ Swap = Swap
  imapM5 _ Swap = return Swap

instance IFunctor5 DropL where
  imap5 _ DropL = DropL
  imapM5 _ DropL = return DropL

instance IFunctor5 DropR where
  imap5 _ DropR = DropR
  imapM5 _ DropR = return DropR

instance IFunctor5 Task where
  imap5 _ (Task f) = Task f
  imapM5 _ (Task f) = return $ Task f

instance IFunctor5 Map where
  imap5 _ (Map c) = Map c
  imapM5 _ (Map c) = return $ Map c

type CircuitF = Id :+: Replicate :+: Then :+: Beside :+: Swap :+: DropL :+: DropR :+: Task :+: Map

{-|
The core type for a Circuit. It takes 5 different type arguments

@
Circuit (inputsStorageTypes  :: [Type -> Type]) (inputsTypes  :: [Type])
        (outputsStorageTypes :: [Type -> Type]) (outputsTypes :: [Type])
        (nInputs :: Nat)
@

@inputStorageTypes@ is a type-list of storage types,
for example @'['Pipeline.DataStore.VariableStore', 'Pipeline.DataStore.CSVStore']@.

@inputTypes@ is a type-list of the types stored in the storage,
for example @'['Int', [('String', 'Float')]]@.

@outputsStorageTypes@ and @inputTypes@ mirror the examples above, but for the outputs instead.

@nInputs@ is a type-level 'Nat' that is the length of all input lists.

-}
type Circuit = IFix5 CircuitF
