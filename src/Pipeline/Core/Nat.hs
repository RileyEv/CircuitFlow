module Pipeline.Core.Nat where


data Nat = Zero | Succ Nat
data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)


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

class SAdd (n :: Nat) (m :: Nat) (nPlusM :: Nat) where
  sAdd :: SNat n -> SNat m -> SNat nPlusM
instance SAdd 'Zero y y where
  sAdd SZero y = y
instance SAdd x 'Zero x where
  sAdd x SZero = x
instance SAdd x y r => SAdd ('Succ x) y ('Succ r) where
  sAdd (SSucc x) y = SSucc (sAdd x y)


type family Add (m :: Nat) (n :: Nat) where
  Add 'Zero n = n
  Add ('Succ x) n = 'Succ (Add x n)

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
