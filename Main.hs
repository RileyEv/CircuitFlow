{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables #-}

module Main where

import Pipeline.Core.Task (Task, DataSource(..), DataSink(..), VariableStore(..), Node(..), functionTask, processGraph, testGraph)

import Data.Typeable (Typeable, cast, typeOf )

    
-- identityTask :: Task String String
-- identityTask source = do
--   input <- fetch source
--   save input

  

-- main' :: IO ()
-- main' = do
--   -- This task will read from stdin and the print it out to stdout
--   res  <- identityTask IOIn :: IO (IOStore String)

--   -- This task will use a variable store to read input from and then output it to stdin
--   res' <- identityTask (Var "Hello World!") :: IO (IOStore String)

--   -- This task will use a variable store to read the input and save the output
--   (Var res'') <- identityTask (Var "Hello again!") :: IO (VariableStore String)
--   print res''

--   -- This turns a haskell function into Task
--   res <- functionTask (+1) (Var 1) :: IO (IOStore Int)

--   return () 


main :: IO ()
main = do
  graph <- processGraph testGraph (Var ("100" :: String))
  let dn = last graph
  y <- test dn
  print (typeOf y)
  print y
  print (length graph)

test :: Node -> IO Int
test (DataNode d) = test' d

test' :: Typeable a => VariableStore a -> IO Int
test' (Var x) = case cast x of
  Just x1 -> return x1
  Nothing -> error "" 

getInt :: Typeable a => a -> Int
getInt x = case cast x of
  Just x1 -> x1
  Nothing -> error "error"
