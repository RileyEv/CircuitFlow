module Pipeline.Internal.Core.UUID (JobUUID(..), TaskUUID(..), genJobUUID, genTaskUUID, nilJobUUID, genUnusedTaskUUID) where

import           Control.Concurrent  (ThreadId)
import           Control.Monad.Loops (iterateUntil)
import           Data.UUID           (UUID, nil)
import           Data.UUID.V4        (nextRandom)
import qualified Data.Map as M       (notMember, Map)

newtype JobUUID = JobUUID { unJobUUID :: UUID } deriving (Eq, Ord)
newtype TaskUUID = TaskUUID { unTaskUUID :: UUID } deriving (Eq, Ord)

instance Show JobUUID where
  show (JobUUID uuid) = show uuid

instance Show TaskUUID where
  show (TaskUUID uuid) = show uuid

genJobUUID :: IO JobUUID
genJobUUID = JobUUID <$> nextRandom

genTaskUUID :: IO TaskUUID
genTaskUUID = TaskUUID <$> nextRandom

nilJobUUID :: JobUUID
nilJobUUID = JobUUID nil

genUnusedTaskUUID :: M.Map TaskUUID ThreadId -> IO TaskUUID
genUnusedTaskUUID m = iterateUntil (`M.notMember` m) genTaskUUID
