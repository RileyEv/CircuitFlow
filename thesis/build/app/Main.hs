module Main where

import           Pipeline
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
  f :: HList' '[FileStore] '[String] -> FileStore String -> IO (FileStore String)
  f (HCons' (FileStore fInName) HNil') fOut@(FileStore fOutName) = do
    callCommand ("lhs2tex -o " ++ fOutName ++ " " ++ fInName ++ " > lhs2tex.log")
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
  f :: HList' '[FileStore] '[String] -> FileStore String -> IO (FileStore String)
  f (HCons' (FileStore fInName) HNil') fOut@(FileStore fOutName) = do
    callCommand
      ("texfot --no-stderr latexmk -interaction=nonstopmode -pdf -no-shell-escape -bibtex -jobname=dissertation "
      ++ fInName
      )
    return fOut

main :: IO ()
main = do
  n <- startNetwork buildDissPipeline

  input (HCons' (FileStore "dissertation.lhs") HNil') n

  o <- output n
  print o

  stopNetwork n
  print "Done"
