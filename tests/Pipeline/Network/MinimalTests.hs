{-# LANGUAGE TypeOperators, FlexibleInstances #-}
module Pipeline.Network.MinimalTests
  ( minimalTests
  ) where

import           Pipeline
import           Pipeline.Nat (SNat (..))
import           Pipeline.Network.Helper
import           Pipeline.Network.HelperCircuit
import           Pipeline.Internal.Core.UUID
import           Prelude                        hiding (id, replicate, (<>))
import           Test.Tasty
import           Test.Tasty.HUnit


-- This is bad... !
instance Show (HList '[]) where
  show _ = "empty"

instance Show (HList (x ': xs)) where
  show _ = "cons"

minimalTests :: TestTree
minimalTests = testGroup
  "Minimal Examples"
  [ idTests
  , replicate2Tests
  , replicate3Tests
  , thenTests
  , besideTests
  , swapTests
  , dropLTests
  , dropRTests
  , functionTaskTests
  , multiInputTaskTests
  , mapTests
  , replicateInputsTests
  ]


varWith :: a -> IO (Var a)
varWith x = do
  var <- emptyVar
  save var x
  return var 


-- Tests for the 'Id' constructor
idCircuit
  :: Circuit
       '[Var]
       '[Int]
       '[Var]
       '[Int]
       N1
idCircuit = id

idTests :: TestTree
idTests = testGroup
  "id should"
  [ testCase "return the same value input" $ do
      var <- varWith 0
      let i = HCons' var HNil'
      (Right o) <- singleInputTest idCircuit i
      out <- fetch' o
      out @?= HCons 0 HNil
  ]

-- Tests for the 'Replicate' constructor
replicate2Circuit
  :: Circuit
       '[Var]
       '[Int]
       '[Var , Var]
       '[Int , Int]
       N1
replicate2Circuit = replicate2

replicate2Tests :: TestTree
replicate2Tests = testGroup
  "replicate2 should"
  [ testCase "return a duplicated input value" $ do
      var <- varWith 0
      let i = HCons' var HNil'
      (Right o) <- singleInputTest replicate2Circuit i
      out <- fetch' o
      out @?= HCons 0 (HCons 0 HNil)
  ]
  
replicate3Circuit
  :: Circuit
       '[Var]
       '[Int]
       '[Var , Var , Var]
       '[Int , Int , Int]
       N1
replicate3Circuit = replicateN (SSucc (SSucc (SSucc SZero)))

replicate3Tests :: TestTree
replicate3Tests = testGroup
  "(replicateN 3) should"
  [ testCase "return a trupled input value" $ do
      var <- varWith 0
      let i = HCons' var HNil'
      (Right o) <- singleInputTest replicate3Circuit i
      out <- fetch' o
      out @?= HCons 0 (HCons 0 (HCons 0 HNil))
  ]


replicateInputsCircuit
  :: Circuit
       '[Var , Var]
       '[Int , Int]
       '[Var , Var , Var , Var]
       '[Int , Int , Int , Int]
       N2
replicateInputsCircuit = replicateInputs (SSucc (SSucc SZero))

replicateInputsTests :: TestTree
replicateInputsTests = testGroup
  "(replicateN 3) should"
  [ testCase "return a trupled input value" $ do
      var1 <- varWith 0
      var2 <- varWith 2
      let i = HCons' var1 (HCons' var2 HNil')
      (Right o) <- singleInputTest replicateInputsCircuit i
      out <- fetch' o
      out @?= HCons 0 (HCons 0 (HCons 2 (HCons 2 HNil)))
  ]
 
-- Tests for the 'Then' constructor
thenCircuit
  :: Circuit
       '[Var]
       '[Int]
       '[Var]
       '[Int]
       N1
thenCircuit = id <-> id

thenTests :: TestTree
thenTests = testGroup
  "<-> should"
  [ testCase "return a return the same input value" $ do
      var <- varWith 0
      let i = HCons' var HNil'
      (Right o) <- singleInputTest thenCircuit i
      out <- fetch' o
      out @?= HCons 0 HNil
  ]

-- Tests for the 'Beside' constructor
besideCircuit
  :: Circuit
       '[Var , Var]
       '[Int , String]
       '[Var , Var]
       '[Int , String]
       N2
besideCircuit = id <> id

besideTests :: TestTree
besideTests = testGroup
  "<> should"
  [ testCase "return a return the same input value" $ do
      var1 <- varWith 0
      var2 <- varWith "abc"
      let i = HCons' var1 (HCons' var2 HNil')
      (Right o) <- singleInputTest besideCircuit i
      out <- fetch' o
      out @?= HCons 0 (HCons "abc" HNil)
  ]


-- Tests for the 'Swap' constructor
swapCircuit
  :: Circuit
       '[Var , Var]
       '[Int , String]
       '[Var , Var]
       '[String , Int]
       N2
swapCircuit = swap

swapTests :: TestTree
swapTests = testGroup
  "swap should"
  [ testCase "return a return the same input value" $ do
      var1 <- varWith 0
      var2 <- varWith "abc"
      let i = HCons' var1 (HCons' var2 HNil')
      (Right o) <- singleInputTest swapCircuit i
      out <- fetch' o
      out @?= HCons "abc" (HCons 0 HNil)
  ]


-- Tests for the 'DropL' constructor
dropLCircuit
  :: Circuit
       '[Var , Var]
       '[Int , String]
       '[Var]
       '[String]
       N2
dropLCircuit = dropL

dropLTests :: TestTree
dropLTests = testGroup
  "dropL should"
  [ testCase "return the input with the left side dropped" $ do
      var1 <- varWith 0
      var2 <- varWith "abc"
      let i = HCons' var1 (HCons' var2 HNil')
      (Right o) <- singleInputTest dropLCircuit i
      out <- fetch' o
      out @?= HCons "abc" HNil
  ]

-- Tests for the 'DropR' constructor
dropRCircuit
  :: Circuit
       '[Var , Var]
       '[Int , String]
       '[Var]
       '[Int]
       N2
dropRCircuit = dropR

dropRTests :: TestTree
dropRTests = testGroup
  "dropR should"
  [ testCase "return a return the same input value" $ do
      var1 <- varWith 0
      var2 <- varWith "abc"
      let i = HCons' var1 (HCons' var2 HNil')
      (Right o) <- singleInputTest dropRCircuit i
      out <- fetch' o
      out @?= HCons 0 HNil
  ]


-- Tests for the 'Task' constructor
functionTaskTests :: TestTree
functionTaskTests = testGroup
  "functionTask should"
  [ testCase "apply the function to the input value" $ do
      var <- varWith 0
      let i = HCons' var HNil'
      (Right o) <- singleInputTest functionTaskCircuit i
      out <- fetch' o
      out @?= HCons 1 HNil
  ]


multiInputTaskTests :: TestTree
multiInputTaskTests = testGroup
  "swap should"
  [ testCase "apply the function to the input values" $ do
      var1 <- varWith 3
      var2 <- varWith 5
      let i = HCons' var1 (HCons' var2 HNil')
      (Right o) <- singleInputTest multiInputTaskCircuit i
      out <- fetch' o
      out @?= HCons 8 HNil
  ]

mapCircuit
  :: Circuit
       '[Var]
       '[[Int]]
       '[Var]
       '[[Int]]
       N1
mapCircuit = mapC functionTaskCircuit

mapTests :: TestTree
mapTests = testGroup
  "map should"
  [ testCase "map a circuit on the input values" $ do
      var <- varWith [0, 1, 2, 3, 4, 5, 6, 7, 8] 
      let i = HCons' var HNil'
      (Right o) <- singleInputTest mapCircuit i
      out <- fetch' o
      out @?= HCons [1, 2, 3, 4, 5, 6, 7, 8, 9] HNil
  ]
