{-# LANGUAGE UndecidableInstances #-}
module Pipeline.Internal.Common.TypeList where

import           Pipeline.Internal.Common.Nat (Nat (..))

import           Data.Kind                    (Type)

type family Length (l :: [k]) :: Nat where
  Length '[] = 'Zero
  Length (e ': l) = 'Succ (Length l)

type family Take (x :: Nat) (l :: [k]) :: [k] where
  Take 'Zero     l        = '[]
  Take ('Succ n) '[]      = '[]
  Take ('Succ n) (e ': l) = e ': Take n l

type family Drop (x :: Nat) (l :: [k]) :: [k] where
  Drop 'Zero     l        = l
  Drop ('Succ _) '[]      = '[]
  Drop ('Succ n) (e ': l) = Drop n l


type family (:++) (l1 :: [k]) (l2 :: [k]) :: [k] where
  (:++) '[] l = l
  (:++) (e ': l) l' = e ': (l :++ l')

infixr 5 :++


type family Apply (fs :: [Type -> Type]) (as :: [Type]) where
  Apply '[] '[] = '[]
  Apply (f ': fs) (a ': as) = f a ': Apply fs as

type family Replicate (n :: Nat) (a :: k) :: [k] where
  Replicate 'Zero _ = '[]
  Replicate ('Succ n) x = x ': Replicate n x

type family Duplicate (a :: [k]) :: [k] where
  Duplicate '[] = '[]
  Duplicate (x ': xs) = x ': x ': Duplicate xs

type family Swap (n :: Nat) (a :: [k]) :: [k] where
  Swap n     '[] = '[]
  Swap 'Zero l   = l
  Swap ('Succ n) (x ': y ': xs) = y ': x ': Swap n xs


type family Pair (xs :: [k]) :: [[k]] where
  Pair '[] = '[]
  Pair (x ': y ': xs) = '[x, y] ': Pair xs

type family Concat (xss :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat ('[] ': l) = Concat l
  Concat ((x ': xs) ': l) = x ': Concat (xs ': l)

type family Transpose (xss :: [[k]]) :: [[k]] where
  Transpose ('[] ': _) = '[]
  Transpose xss = Heads xss ': Transpose (Tails xss)

type family Heads (xss :: [[k]]) :: [k] where
  Heads '[] = '[]
  Heads ((x ': xs) ': xss) = x ': Heads xss
  
type family Tails (xss :: [[k]]) :: [[k]] where
  Tails '[] = '[]
  Tails ((x ': xs) ': xss) = xs ': Tails xss
