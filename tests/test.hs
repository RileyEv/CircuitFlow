module Main where

import Test.Tasty

import qualified Pipeline.Frontend.Pipe.Tests as Pipe


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Pipeline Tests" [ Pipe.tests
                                   ]
        
