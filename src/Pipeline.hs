{-|
Module      : Pipeline
Description : All the modules needed to build a pipeline
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

All the modules needed to build a pipeline
-}
module Pipeline
  ( module Pipeline.Circuit
  , module Pipeline.DataStore
  , module Pipeline.Nat
  , module Pipeline.Network
  , module Pipeline.Task
  , module Pipeline.Error
  , Task(..)
  , (:+:)(..)
  , IFix7(..)
  ) where

import           Pipeline.Circuit
import           Pipeline.DataStore
import           Pipeline.Error
import           Pipeline.Internal.Common.IFunctor.Modular ((:+:) (..))
import           Pipeline.Internal.Core.CircuitAST         (Task (..))
import           Pipeline.Nat
import           Pipeline.Network
import           Pipeline.Task

import           Pipeline.Internal.Common.IFunctor         (IFix7 (..))
