module Pipeline.Internal.Common.Nat where

data Nat = Zero | Succ Nat

data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)


-- Operations on Nats
type family (m :: Nat) := (n :: Nat) :: Bool where
  ('Succ n) := 'Zero     = 'False
  'Zero     := 'Zero     = 'True
  'Zero     := ('Succ n) = 'False
  ('Succ m) := ('Succ n) = m := n

type family (a :: Nat) :+ (b :: Nat) where
  a :+ 'Zero     = a
  a :+ ('Succ b) = 'Succ (a :+ b)

-- Recovery of SNat from a type-level Nat
class IsNat (n :: Nat) where nat :: SNat n

instance IsNat 'Zero where
  nat = SZero

instance IsNat n => IsNat ('Succ n) where
  nat = SSucc nat



