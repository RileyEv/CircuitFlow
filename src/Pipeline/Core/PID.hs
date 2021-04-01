{-# LANGUAGE PolyKinds #-}

module Pipeline.Core.PID where

import Pipeline.Core.DataStore (DataSource', Apply)
import Pipeline.Core.IFunctor (IFunctor4(..))

import Data.Typeable (Typeable)

type PID = Int


data PIDF (iF :: [* -> *] -> [*] -> [* -> *] -> [*] -> *) (fs :: [* -> *]) (as :: [*]) (g :: [* -> *]) (b :: [*]) where
  PIDF :: (DataSource' fs as,
           DataSource' g b,
           Typeable (Apply fs as),
           Typeable fs, Typeable g,
           Typeable as, Typeable b)
    => PID -> PIDF iF fs as g b

instance IFunctor4 PIDF where
  imap4 _ (PIDF p) = PIDF p
