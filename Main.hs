{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Main where

import Pipeline.Core.Task (Task, DataSource(..), DataSink(..), IOStore(..), VariableStore(..), functionTask)


    


identityTask :: Task String String
identityTask source = do
  input <- fetch source
  save input

  

main :: IO ()
main = do
  -- This task will read from stdin and the print it out to stdout
  res  <- identityTask IOIn :: IO (IOStore String)

  -- This task will use a variable store to read input from and then output it to stdin
  res' <- identityTask (Var "Hello World!") :: IO (IOStore String)

  -- This task will use a variable store to read the input and save the output
  (Var res'') <- identityTask (Var "Hello again!") :: IO (VariableStore String)
  print res''

  -- This turns a haskell function into Task
  res <- functionTask (+1) (Var 1) :: IO (IOStore Int)

  return () 
