module Main where

import Test.Tasty

import qualified Pipeline.Network.Tests as Network




main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Pipeline Tests" [Network.tests]
