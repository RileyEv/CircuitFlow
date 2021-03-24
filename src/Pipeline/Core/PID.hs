{-# LANGUAGE PolyKinds #-}

module Pipeline.Core.PID where

import Pipeline.Core.DataStore (DataSource', DataSource, Apply)
import Pipeline.Core.IFunctor (IFunctor2(..))

import Data.Typeable (Typeable)

type PID = Int


data PIDF (iF :: [*] -> [*] -> *) (fas :: [*]) (gb :: [*]) where
  PIDF :: (fas ~ Apply fs as,
            gb ~ Apply '[g] '[b],
            DataSource' fs as (Apply fs as),
            DataSource g b,
            Typeable (Apply fs as),
            Typeable fs, Typeable g,
            Typeable as, Typeable b)
    => PID -> PIDF iF fas gb

instance IFunctor2 PIDF where
  imap2 _ (PIDF p) = PIDF p
