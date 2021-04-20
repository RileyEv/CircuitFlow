{-# LANGUAGE FunctionalDependencies #-}
module Pipeline.Internal.Core.Error where

import           Control.Monad.Except (MonadError)

newtype ExceptionMessage = ExceptionMessage {unExceptionMessage :: String} deriving (Eq, Show)
data TaskError = TaskError ExceptionMessage
  deriving (Eq, Show)
