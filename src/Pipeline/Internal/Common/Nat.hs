module Pipeline.Internal.Common.Nat where

-- | Datatype for representing natural numbers
data Nat = Zero     -- ^ Natural number for 0
         | Succ Nat -- ^ Natural number for n + 1

data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

-- | Nat equal to 1
type N1 = 'Succ 'Zero
-- | Nat equal to 2
type N2 = 'Succ N1
-- | Nat equal to 3
type N3 = 'Succ N2
-- | Nat equal to 4
type N4 = 'Succ N3
-- | Nat equal to 5
type N5 = 'Succ N4
-- | Nat equal to 6
type N6 = 'Succ N5
-- | Nat equal to 7
type N7 = 'Succ N6
-- | Nat equal to 8
type N8 = 'Succ N7
-- | Nat equal to 9
type N9 = 'Succ N8

-- Operations on Nats
type family (m :: Nat) := (n :: Nat) :: Bool where
  'Succ n := 'Zero   = 'False
  'Zero   := 'Zero   = 'True
  'Zero   := 'Succ n = 'False
  'Succ m := 'Succ n = m := n

type family (a :: Nat) :+ (b :: Nat) where
  a :+ 'Zero   = a
  a :+ 'Succ b = 'Succ (a :+ b)

-- Recovery of SNat from a type-level Nat
class IsNat (n :: Nat) where nat :: SNat n

instance IsNat 'Zero where
  nat = SZero

instance IsNat n => IsNat ('Succ n) where
  nat = SSucc nat



