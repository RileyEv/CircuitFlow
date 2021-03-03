module Pipeline.Frontend.Verify where

import Pipeline.Core.IFunctor (IFix(..))
import Pipeline.Core.Graph (TreeF(..))
import Pipeline.Core.Task (TaskWrap(..))
import Pipeline.Frontend.PID (PID)

import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)

import qualified Data.Map as M (Map, lookup)
import Data.Typeable (typeOf, typeRepArgs)

verifyTree :: (IFix TreeF) PID -> ReaderT (M.Map PID TaskWrap) IO ()
verifyTree (IIn (TreeF _ [])) = return ()
verifyTree (IIn (TreeF x cs)) = do
  forM_ cs f
  forM_ cs verifyTree
  where
    f :: (IFix TreeF) PID -> ReaderT (M.Map PID TaskWrap) IO ()
    f (IIn (TreeF y _)) = do
      m <- ask
      let xf = M.lookup x m
      let yf = M.lookup y m
      verifyNodeType xf yf

verifyNodeType :: Maybe TaskWrap -> Maybe TaskWrap -> ReaderT (M.Map PID TaskWrap) IO ()
verifyNodeType (Just (TaskWrap t)) (Just (TaskWrap t')) = do
  let xArgs = typeRepArgs (typeOf t)
  let yArgs = typeRepArgs (typeOf t')
  if drop 2 xArgs == take 2 yArgs
    then lift $ print "They Match!"
    else error "Types do not match on task" 
verifyNodeType _ _ = error "Not in the map, should have failed before this?"


verify :: (IFix TreeF) PID -> M.Map PID TaskWrap -> IO ()
verify t = runReaderT (verifyTree t)
