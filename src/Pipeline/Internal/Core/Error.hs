{-# LANGUAGE FunctionalDependencies #-}
module Pipeline.Internal.Core.Error where

import           Control.Monad.Except (MonadError)

newtype TaskName = TaskName {unTaskName :: String} deriving (Eq, Show)
newtype ExceptionMessage = ExceptionMessage {unExceptionMessage :: String} deriving (Eq, Show)
data TaskError = TaskError TaskName ExceptionMessage
  deriving (Eq, Show)
