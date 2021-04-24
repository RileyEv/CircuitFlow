module Main where

import           Control.Monad.Trans (lift)
import           Pipeline
import           Prelude             hiding (read)
import           System.Process

buildDissPipeline
  :: Circuit
       '[FileStore]
       '[String]
       '[FileStore String]
       '[FileStore]
       '[String]
       '[FileStore String]
       N1
buildDissPipeline = lhs2TexTask <-> buildTexTask

lhs2TexTask
  :: Circuit
       '[FileStore]
       '[String]
       '[FileStore String]
       '[FileStore]
       '[String]
       '[FileStore String]
       N1
lhs2TexTask = task f (FileStore "dissertation.tex")
 where
  f
    :: UUID
    -> HList' '[FileStore] '[String]
    -> FileStore String
    -> ExceptT SomeException IO (FileStore String)
  f _ (HCons' (FileStore fInName) HNil') fOut@(FileStore fOutName) = do
    lift (callCommand ("lhs2tex -o " ++ fOutName ++ " " ++ fInName ++ " > lhs2tex.log"))
    return fOut

buildTexTask
  :: Circuit
       '[FileStore]
       '[String]
       '[FileStore String]
       '[FileStore]
       '[String]
       '[FileStore String]
       N1
buildTexTask = task f (FileStore "dissertation.pdf")
 where
  f
    :: UUID
    -> HList' '[FileStore] '[String]
    -> FileStore String
    -> ExceptT SomeException IO (FileStore String)
  f _ (HCons' (FileStore fInName) HNil') fOut@(FileStore _) = do
    lift
      (callCommand
        ("texfot --no-stderr latexmk -interaction=nonstopmode -pdf -no-shell-escape -bibtex -jobname=dissertation "
        ++ fInName
        )
      )
    return fOut

main :: IO ()
main = do
  n <-
    startNetwork buildDissPipeline :: IO
      ( BasicNetwork
          '[FileStore]
          '[String]
          '[FileStore String]
          '[FileStore]
          '[String]
          '[FileStore String]
      )

  input_ (HCons' (FileStore "dissertation.lhs") HNil') n

  o <- read n
  print o

  stopNetwork n
  print "Done"
