module Pipeline.Internal.Core.Error where

-- | A wrapper for an error message
newtype ExceptionMessage = ExceptionMessage {unExceptionMessage :: String} deriving (Eq, Show)

-- | Standard type of an error that is propagated through the network.
newtype TaskError = TaskError ExceptionMessage
  deriving (Eq, Show)
