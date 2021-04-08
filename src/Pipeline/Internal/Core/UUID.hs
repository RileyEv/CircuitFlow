module Pipeline.Internal.Core.UUID where

import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)


type UUID = String

genUUID :: IO UUID
genUUID = toString <$> nextRandom
