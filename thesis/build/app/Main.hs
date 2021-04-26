{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Main where

import           Control.Monad.Trans (lift)
import           Data.Yaml           (FromJSON (..))
import           Data.Yaml.Config    (ignoreEnv, loadYamlSettings)
import           Pipeline
import           Prelude             hiding (read)
import           System.FilePath     ((-<.>))
import           System.Process

buildDissPipeline
  :: String
  -> Circuit
       '[VariableStore]
       '[[String]]
       '[VariableStore [String]]
       '[VariableStore]
       '[String]
       '[VariableStore String]
       N1
buildDissPipeline name = mapC lhs2TexTask Empty <-> buildTexTask name

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
  :: String
  -> Circuit
       '[VariableStore]
       '[[String]]
       '[VariableStore [String]]
       '[VariableStore]
       '[String]
       '[VariableStore String]
       N1
buildTexTask name = task f Empty
 where
  f
    :: UUID
    -> HList' '[VariableStore] '[[String]]
    -> VariableStore String
    -> ExceptT SomeException IO (VariableStore String)
  f mainFileName (HCons' (Var _) HNil') _ = do
    lift
      (callCommand
        ("texfot --no-stderr latexmk -interaction=nonstopmode -pdf -no-shell-escape -bibtex -jobname="
        ++ name
        ++ " "
        ++ mainFileName
        )
      )
    return (Var "dissertation.pdf")


files :: [String]
files = ["dissertation.lhs", "introduction.lhs", "background.lhs"]

data Config = Config
  { mainFile   :: FilePath
  , outputName :: String
  , lhsFiles   :: [FilePath]
  }
  deriving (Generic, FromJSON, Show)

loadConfig :: IO Config
loadConfig = loadYamlSettings ["dissertation.tex-build"] [] ignoreEnv


main :: IO ()
main = do
  config <- loadConfig
  n      <-
    startNetwork (buildDissPipeline (outputName config)) :: IO
      ( BasicNetwork
          '[VariableStore]
          '[[String]]
          '[VariableStore [String]]
          '[VariableStore]
          '[String]
          '[VariableStore String]
      )

  write (mainFile config -<.> "tex") (HCons' (Var (lhsFiles config)) HNil') n

  o <- read n
  print o

  stopNetwork n
  print "Done"
