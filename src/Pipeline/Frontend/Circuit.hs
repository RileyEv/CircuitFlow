{-# LANGUAGE MultiParamTypeClasses
           , TypeFamilyDependencies
           , PolyKinds
           , DataKinds
           , AllowAmbiguousTypes #-}
module Pipeline.Frontend.Circuit where

-- import Pipeline.Core.DataStore (VariableStore(..))
-- import Pipeline.Core.Task



data IOList (xs :: [*]) where
  IOCons :: IO x -> IOList xs -> IOList (x ': xs)
  IONil :: IOList '[]

data HList (xs :: [*]) where
  HCons :: x -> HList xs -> HList (x ': xs)
  HNil :: HList '[]
  
-- data family HList (l :: [*])
-- data instance HList '[] = HNil
-- data instance HList (x ': xs) = x `HCons` HList xs

type family Apply (fs :: [* -> *]) (as :: [*]) = fas | fas -> fs as where
  Apply '[] '[] = '[]
  Apply (f ': fs) (a ': as) = f a ': Apply fs as


class (xs ~ Apply fs as) => DataSource' (fs :: [* -> *]) (as :: [*]) (xs :: [*]) where
  -- | Fetch the value stored in the 'DataSource'
  -- fetch :: f a -> IO a
  fetch' :: HList xs -> IOList as
  -- | Save a value into the 'DataStore'
  --   First argument depends on the instance. It may be 'empty' or it could be a pointer to a storage location.
  -- save :: f a -> a -> IO (f a)
  save' :: HList xs -> HList as -> IOList xs

-- for the user to define.
class DataSource f a where
  fetch :: f a -> IO a
  save :: f a -> a -> IO (f a)


instance (x ~ f a, DataSource f a) => DataSource' '[f] '[a] '[x] where
  fetch' (HCons x HNil) = IOCons (fetch x) IONil
  save' = undefined

instance (x ~ f a, DataSource f a, DataSource' fs as xs) => DataSource' (f ': fs) (a ': as) (x ': xs) where
  fetch' (HCons x xs) = IOCons (fetch x) (fetch' xs)
  save' = undefined




-- instance DataStore '[f] '[a] where




-- data Circuit i o where
--   ID     :: DataSource f a => Circuit (f a) (f a)
--   Apply  :: (DataSource f a, DataSource g b) => Task f a g b -> Circuit (f a) (g b)
--   Branch :: DataSource f a => Circuit (f a) (f a, f a)
--   Then   :: (DataSource f a, DataSource g b, DataSource h c) => Circuit (f a) (g b) -> Circuit (g b) (h c) -> Circuit (f a) (h c)
--   Beside :: (DataSource f a, DataSource g b, DataSource h c, DataSource i d) => Circuit (f a) (g b) -> Circuit (h c) (i d) -> Circuit (f a, h c) (g b, i d)
--   Swap   :: (DataSource f a, DataSource g b) => Circuit (f a, g b)  (g b, f a)
--   DropL  :: (DataSource f a, DataSource g b) => Circuit (f a, g b) (g b)
--   DropR  :: (DataSource f a, DataSource g b) => Circuit (f a, g b) (f a)

-- id :: DataSource f a => Circuit (f a) (f a)
-- id = ID

-- apply :: (DataSource f a, DataSource g b) => Task f a g b -> Circuit (f a) (g b)
-- apply = Apply

-- branch :: (DataSource f a) => Circuit (f a) (f a, f a)
-- branch = Branch

-- (<..>) :: (DataSource f a, DataSource g b, DataSource h c) => Circuit (f a) (g b) -> Circuit (g b) (h c) -> Circuit (f a) (h c)
-- (<..>) = Then

-- infixr 4 <..> 


-- (<&>) :: (DataSource f a, DataSource g b, DataSource h c, DataSource i d) => Circuit (f a) (g b) -> Circuit (h c) (i d) -> Circuit (f a, h c) (g b, i d)
-- (<&>) = Beside

-- infixr 5 <&>

-- swap :: (DataSource f a, DataSource g b) => Circuit (f a, g b) (g b, f a)
-- swap = Swap


-- plus1Task :: Task VariableStore Int VariableStore Int
-- plus1Task = functionTask (+1) Empty

-- showTask :: Task VariableStore Int VariableStore String
-- showTask = functionTask (show) Empty

-- example :: Circuit (VariableStore Int) (VariableStore String, VariableStore Int)
-- example = branch
--           <..>
--           apply plus1Task <&> id
--           <..>
--           apply plus1Task <&> apply showTask
--           <..>
--           swap






data VariableStore a = Var a | VEmpty

-- instance DataSource VariableStore a () where
--   fetch (Var x) = return (x, ())
--   fetch Empty   = error "empty source"
--   save _ (x, _) = return (Var x)

-- instance DataSource (f :&&: g) a b where
--   fetch (x :&&: y) = do
--     x' <- fetch x 
--     y' <- fetch y
--     return (x', y')
--   save (x :&&: y) (x', y') = do
--     x'' <- save x x'
--     y'' <- save y y'
--     return x'' :&&: y''
  

-- example' :: Circuit (VariableStore Int) (VariableStore (String, Int))
-- example' = branch
--            <..>
--            (apply (+1) <..> apply (+1)) <&> apply (show :: Int -> String)
--            <..>
--            swap

  
-- example = Then (Then
--                  Branch
--                  (Apply (+1) `Beside` ID))
--                (Apply (+1) `Beside` Apply (+1))
