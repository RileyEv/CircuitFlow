{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, UndecidableInstances #-}
module Pipeline.Core.Nat where

import Data.Type.Equality ((:~:)(..), gcastWith)


data Nat = Zero | Succ Nat

data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)


type family Pred n where
  Pred 'Zero     = 'Zero
  Pred ('Succ n) = n

-- Operations on Nats
type family (m :: Nat) :< (n :: Nat) :: Bool where
  m         :< 'Zero     = 'False
  'Zero     :< ('Succ n) = 'True
  ('Succ m) :< ('Succ n) = m :< n

type family (m :: Nat) :<= (n :: Nat) :: Bool where
  ('Succ n) :<= 'Zero     = 'False
  'Zero     :<= 'Zero     = 'True
  'Zero     :<= ('Succ n) = 'True
  ('Succ m) :<= ('Succ n) = m :<= n
  
type family (m :: Nat) := (n :: Nat) :: Bool where
  ('Succ n) := 'Zero     = 'False
  'Zero     := 'Zero     = 'True
  'Zero     := ('Succ n) = 'False
  ('Succ m) := ('Succ n) = m := n

type family (x :: Bool) :&& (y :: Bool) :: Bool where
  'True  :&& 'True  = 'True
  _      :&& 'False = 'False
  'False :&& _      = 'False
  
class SMinus (n :: Nat) (m :: Nat) (nMinusM :: Nat) where
  sMinus :: SNat n -> SNat m -> SNat nMinusM
instance SMinus x 'Zero x where
  sMinus x SZero = x
instance SMinus x y r => SMinus ('Succ x) ('Succ y) r where
  sMinus (SSucc x) (SSucc y) = sMinus x y
instance SMinus 'Zero ('Succ x) r where
  sMinus = error "Nat cant be negative"


type family (+) (a :: Nat) (b :: Nat) where
  a + 'Zero     = a
  a + ('Succ b) = 'Succ (a + b)

type family (-) (a :: Nat) (b :: Nat) where
  a - 'Zero   = a
  a - 'Succ b = Pred (a - b)

testEquality :: ('Zero + 'Zero) :~: 'Zero
testEquality = Refl

testEquality' :: (a + 'Zero) :~: a
testEquality' = Refl

testEquality'' :: (a + 'Succ b) :~: 'Succ (a + b)
testEquality'' = Refl

plusLeftId :: SNat a -> ('Zero + a) :~: a
plusLeftId SZero = Refl
plusLeftId (SSucc n) = gcastWith (plusLeftId n) Refl


given1 :: SNat a -> (a + 'Zero) :~: a
given1 _ = Refl

given2 :: SNat a -> SNat b -> (a + 'Succ b) :~: 'Succ (a + b)
given2 _ _ = Refl

plusRightId :: SNat a -> (a + 'Zero) :~: a
plusRightId = given1

-- Associativity
(!+) :: SNat n -> SNat m -> SNat (n + m)
n !+ SZero = n
n !+ (SSucc m) = SSucc (n !+ m)

(==>) :: a :~: b -> b :~: c -> a :~: c
Refl ==> Refl = Refl


plusAssoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZero =
  let
    step1 :: SNat x -> SNat y -> ((x + y) + 'Zero) :~: (x + y)
    step1 x y = gcastWith (given1 (x !+ y)) Refl

    step2 :: SNat x -> SNat y -> (x + y) :~: (x + (y + 'Zero))
    step2 _ y = gcastWith (given1 y) Refl -- Says that y :~: (y + 'Zero)
  in step1 a b ==> step2 a b
plusAssoc a b (SSucc c) =
  let
    step1 :: SNat x -> SNat y -> SNat ('Succ z) -> (x + y) + 'Succ z :~: 'Succ ((x + y) + z)
    step1 x y z = gcastWith (given2 (x !+ y) z) Refl

    step2 :: SNat x -> SNat y -> SNat z -> 'Succ ((x + y) + z) :~: 'Succ (x + (y + z))
    step2 x y z = gcastWith (plusAssoc x y z) Refl

    step3 :: SNat x -> SNat y -> SNat z -> 'Succ (x + (y + z)) :~: x + 'Succ (y + z)
    step3 x y z = gcastWith (given2 x (y !+ z)) Refl

    step4 :: SNat x -> SNat y -> SNat z -> x + 'Succ (y + z) :~: x + (y + 'Succ z)
    step4 _ y z = gcastWith (given2 y z) Refl
  
  in step1 a b (SSucc c) ==> step2 a b c ==> step3 a b c ==> step4 a b c


plusAssoc' :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc' _ _ SZero = Refl
plusAssoc' a b (SSucc c) = gcastWith (plusAssoc a b c) Refl


-- Commutativity
-- a + b = b + a

plusComm :: SNat a -> SNat b -> (a + b) :~: (b + a)
plusComm SZero SZero = Refl
plusComm a     SZero = gcastWith (plusLeftId a) Refl
plusComm SZero (SSucc SZero) = Refl
plusComm (SSucc a) (SSucc SZero) = gcastWith (plusComm a (SSucc SZero)) Refl
plusComm a (SSucc b) =
  let
    proof :: forall a b. SNat a -> SNat b -> a + 'Succ b :~: 'Succ b + a
    proof x y = p1 ==> p2 ==> p3 ==> p4 ==> p5 ==> p6 ==> p7
      where
        p1 :: a + 'Succ b :~: a + (b + 'Succ 'Zero)
        p1 = gcastWith (given2 y (SSucc SZero)) Refl
        p2 :: a + (b + 'Succ 'Zero) :~: (a + b) + 'Succ 'Zero
        p2 = gcastWith (plusAssoc x y (SSucc SZero)) Refl
        p3 :: (a + b) + 'Succ 'Zero :~: (b + a) + 'Succ 'Zero
        p3 = gcastWith (plusComm x y) Refl
        p4 :: (b + a) + 'Succ 'Zero :~: b + (a + 'Succ 'Zero)
        p4 = gcastWith (plusAssoc y x (SSucc SZero)) Refl
        p5 :: b + (a + 'Succ 'Zero) :~: b + ('Succ 'Zero + a)
        p5 = gcastWith (plusComm x (SSucc SZero)) Refl
        p6 :: b + ('Succ 'Zero + a) :~: (b + 'Succ 'Zero) + a
        p6 = gcastWith (plusAssoc y (SSucc SZero) x) Refl
        p7 :: (b + 'Succ 'Zero) + a :~: 'Succ b + a
        p7 = gcastWith (given2 y (SSucc SZero)) Refl
  in proof a b

-- How to use the proofs
data Vec :: Nat -> * -> * where
  V0 :: Vec 'Zero a
  (:>) :: a -> Vec n a -> Vec ('Succ n) a
infixr 5 :>

append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZero     m V0        ys = gcastWith (plusLeftId m) ys
append (SSucc n) m (x :> xs) ys = gcastWith (proof n m) $ x :> append n m xs ys
  where
    proof :: forall x y. SNat x -> SNat y -> ('Succ x + y) :~: 'Succ (x + y)
    proof x y = p1 ==> p2 ==> p3
      where
        p1 :: ('Succ x + y) :~: (y + 'Succ x) --comm
        p1 = gcastWith (plusComm (SSucc x) y) Refl
        p2 :: (y + 'Succ x) :~: 'Succ (y + x) -- (2)
        p2 = gcastWith (given2 y x) Refl
        p3 :: 'Succ (y + x) :~: 'Succ (x + y) -- comm
        p3 = gcastWith (plusComm y x) Refl




-- type family Sum (l :: [Nat]) :: Nat where
--   Sum '[]      = 'Zero
--   Sum (e ': l) = Add e (Sum l)
  
type family Length (l :: [k]) :: Nat where
  Length '[] = 'Zero
  Length (e ': l) = 'Succ (Length l)
  
type family Take (x :: Nat) (l :: [k]) :: [k] where
  Take 'Zero l = '[]
  Take ('Succ n) (e ': l) = e ': Take n l
  
type family Drop (x :: Nat) (l :: [k]) :: [k] where
  Drop 'Zero l = l
  Drop ('Succ n) (e ': l) = Drop n l
