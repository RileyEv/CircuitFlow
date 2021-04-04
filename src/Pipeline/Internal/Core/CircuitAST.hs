module Pipeline.Internal.Core.CircuitAST (
  Circuit,
  Id(..),
  Replicate(..),
  Then(..),
  Beside(..),
  Swap(..),
  DropL(..),
  DropR(..),
  Task(..)
) where  

import Pipeline.Internal.Core.DataStore (DataStore', DataStore)
import Pipeline.Internal.Core.PipeList (AppendP)
import Pipeline.Internal.Common.Nat (Nat(..), (:+), (:=), IsNat(..))
import Pipeline.Internal.Common.IFunctor (IFix7(..), IFunctor7(..))
import Pipeline.Internal.Common.IFunctor.Modular ((:+:)(..))
import Pipeline.Internal.Common.HList (HList'(..))
import Pipeline.Internal.Common.TypeList (Length, Drop, Take, Apply, (:++))

import Data.Kind (Type)


data Id (iF :: [Type -> Type] -> [Type] -> [Type] -> [Type -> Type] -> [Type] -> [Type] -> Nat -> Type)
        (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type])
        (outputsS :: [Type -> Type]) (outputsT :: [Type]) (outputsA :: [Type]) (ninputs :: Nat) where
  Id :: (DataStore' '[inputS] '[inputT])
    => Id iF '[inputS] '[inputT] '[inputS inputT] '[inputS] '[inputT] '[inputS inputT] ('Succ 'Zero)

data Replicate (iF :: [Type -> Type] -> [Type] -> [Type] -> [Type -> Type] -> [Type] -> [Type] -> Nat -> Type)
               (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type])
               (outputsS :: [Type -> Type]) (outputsT :: [Type]) (outputsA :: [Type]) (ninputs :: Nat) where
  Replicate :: (DataStore' '[f] '[a])
    => Replicate iF '[f] '[a] '[f a] '[f, f] '[a, a] '[f a, f a] ('Succ 'Zero)

data Then (iF :: [Type -> Type] -> [Type] -> [Type] -> [Type -> Type] -> [Type] -> [Type] -> Nat -> Type)
          (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type])
          (outputsS :: [Type -> Type]) (outputsT :: [Type]) (outputsA :: [Type]) (ninputs :: Nat) where
  Then :: (DataStore' fs as, DataStore' gs bs, DataStore' hs cs)
    => iF fs as (Apply fs as) gs bs (Apply gs bs) nfs
    -> iF gs bs (Apply gs bs) hs cs (Apply hs cs) ngs
    -> Then iF fs as (Apply fs as) hs cs (Apply hs cs) nfs

data Beside (iF :: [Type -> Type] -> [Type] -> [Type] -> [Type -> Type] -> [Type] -> [Type] -> Nat -> Type)
            (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type])
            (outputsS :: [Type -> Type]) (outputsT :: [Type]) (outputsA :: [Type]) (ninputs :: Nat) where
  Beside :: (DataStore' fs as,
             DataStore' gs bs,
             DataStore' hs cs,
             DataStore' is ds,
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
    -> Beside iF (fs :++ hs) (as :++ cs) (Apply fs as :++ Apply hs cs)
                 (gs :++ is) (bs :++ ds) (Apply gs bs :++ Apply is ds) (nfs :+ nhs)

data Swap (iF :: [Type -> Type] -> [Type] -> [Type] -> [Type -> Type] -> [Type] -> [Type] -> Nat -> Type)
          (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type])
          (outputsS :: [Type -> Type]) (outputsT :: [Type]) (outputsA :: [Type]) (ninputs :: Nat) where
  Swap :: (DataStore' '[f, g] '[a, b])
    => Swap iF '[f, g] '[a, b] '[f a, g b] '[g, f] '[b, a] '[g b, f a] ('Succ ('Succ 'Zero))

data DropL (iF :: [Type -> Type] -> [Type] -> [Type] -> [Type -> Type] -> [Type] -> [Type] -> Nat -> Type)
           (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type])
           (outputsS :: [Type -> Type]) (outputsT :: [Type]) (outputsA :: [Type]) (ninputs :: Nat) where
  DropL :: (DataStore' '[f, g] '[a, b])
    => DropL iF '[f, g] '[a, b] '[f a, g b] '[g] '[b] '[g b] ('Succ ('Succ 'Zero))

data DropR (iF :: [Type -> Type] -> [Type] -> [Type] -> [Type -> Type] -> [Type] -> [Type] -> Nat -> Type)
           (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type])
           (outputsS :: [Type -> Type]) (outputsT :: [Type]) (outputsA :: [Type]) (ninputs :: Nat) where
  DropR :: (DataStore' '[f, g] '[a, b])
    => DropR iF '[f, g] '[a, b] '[f a, g b] '[f] '[a] '[f a] ('Succ ('Succ 'Zero))


data Task (iF :: [Type -> Type] -> [Type] -> [Type] -> [Type -> Type] -> [Type] -> [Type] -> Nat -> Type)
           (inputsS :: [Type -> Type]) (inputsT :: [Type]) (inputsA :: [Type])
           (outputsS :: [Type -> Type]) (outputsT :: [Type]) (outputsA :: [Type]) (ninputs :: Nat) where
  Task :: (Length outputsS := 'Succ 'Zero ~ 'True,
           outputsS ~ '[g'], outputsT ~ '[b'], outputsA ~ '[g' b'],
           DataStore' inputsS inputsT,
           DataStore g' b', Eq (g' b'), Show (g' b'))
       => (HList' inputsS inputsT -> g' b' -> IO (g' b'))
       -> g' b'
       -> Task iF inputsS inputsT (Apply inputsS inputsT) outputsS outputsT outputsA (Length inputsS)

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

instance IFunctor7 Task where
  imap7 _ (Task f output) = Task f output
  
type CircuitF = Id :+: Replicate :+: Then :+: Beside :+: Swap :+: DropL :+: DropR :+: Task

type Circuit = IFix7 CircuitF
