module Pipeline.Internal.Common.TypeList where

import Pipeline.Internal.Common.Nat (Nat(..))

import Data.Kind (Type)

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
