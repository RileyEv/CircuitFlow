module Pipeline.Internal.Core.UUID where

import           Data.UUID    (toString)
import           Data.UUID.V4 (nextRandom)

-- | A synonym for a Unique Id for a task
type UUID = String

genUUID :: IO UUID
genUUID = toString <$> nextRandom
