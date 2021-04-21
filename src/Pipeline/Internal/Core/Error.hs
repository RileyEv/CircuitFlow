module Pipeline.Internal.Core.Error where


newtype ExceptionMessage = ExceptionMessage {unExceptionMessage :: String} deriving (Eq, Show)
data TaskError = TaskError ExceptionMessage
  deriving (Eq, Show)
