{-|
Module      : Pipeline.Error
Description : Handling errors nicely
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

This package contains the tools needed to throw errors inside a Task.

Errors will be propagated through the network.

-}
module Pipeline.Error
  ( TaskError(..)
  , ExceptionMessage(..)
  , ExceptT
  , SomeException
  , throwE
  , throw
  , throwIO
  ) where


import           Control.Exception            (SomeException, throw, throwIO)
import           Control.Monad.Trans.Except   (ExceptT, throwE)
import           Pipeline.Internal.Core.Error
