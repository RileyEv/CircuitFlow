module Main where

import           Control.Monad.Trans (lift)
import           Pipeline
import           Prelude             hiding (read)
import           System.FilePath     ((-<.>))
import           System.Process


buildDissPipeline
  :: Circuit
       '[VariableStore]
       '[[String]]
       '[VariableStore [String]]
       '[VariableStore]
       '[String]
       '[VariableStore String]
       N1
buildDissPipeline = mapC lhs2TexTask Empty <-> buildTexTask

lhs2TexTask
  :: Circuit
       '[VariableStore]
       '[String]
       '[VariableStore String]
       '[VariableStore]
       '[String]
       '[VariableStore String]
       N1
lhs2TexTask = task f (Var "dissertation.tex")
 where
  f
    :: UUID
    -> HList' '[VariableStore] '[String]
    -> VariableStore String
    -> ExceptT SomeException IO (VariableStore String)
  f _ (HCons' (Var fInName) HNil') _ = do
    let fOutName = fInName -<.> "tex"
    lift (callCommand ("lhs2tex -o " ++ fOutName ++ " " ++ fInName ++ " > lhs2tex.log"))
    return (Var fOutName)

buildTexTask
  :: Circuit
       '[VariableStore]
       '[[String]]
       '[VariableStore [String]]
       '[VariableStore]
       '[String]
       '[VariableStore String]
       N1
buildTexTask = task f Empty
 where
  f
    :: UUID
    -> HList' '[VariableStore] '[[String]]
    -> VariableStore String
    -> ExceptT SomeException IO (VariableStore String)
  f _ (HCons' (Var _) HNil') _ = do
    lift
      (callCommand
        "texfot --no-stderr latexmk -interaction=nonstopmode -pdf -no-shell-escape -bibtex -jobname=dissertation dissertation.tex"
      )
    return (Var "dissertation.pdf")


files :: [String]
files = ["dissertation.lhs", "introduction.lhs", "background.lhs"]

main :: IO ()
main = do
  n <-
    startNetwork buildDissPipeline :: IO
      ( BasicNetwork
          '[VariableStore]
          '[[String]]
          '[VariableStore [String]]
          '[VariableStore]
          '[String]
          '[VariableStore String]
      )

  input_ (HCons' (Var files) HNil') n

  o <- read n
  print o

  stopNetwork n
  print "Done"
