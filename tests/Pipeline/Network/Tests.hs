module Pipeline.Network.Tests where

import Test.Tasty

import Pipeline.Network.MinimalTests (minimalTests)


tests :: TestTree
tests = testGroup "Network" [minimalTests]
