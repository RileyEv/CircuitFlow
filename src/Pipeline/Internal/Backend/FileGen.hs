
{-|
Module      : Pipeline.Internal.Backend.FileGen
Description : Handles files
Copyright   : (c) Riley Evans, 2020
License     : BSD 3-Clause
Maintainer  : haskell@rly.rocks

-}
module Pipeline.Internal.Backend.FileGen (createNewFile) where

import           Pipeline.Internal.Core.UUID (TaskUUID, JobUUID)
import           System.FilePath             ((</>), (<.>), dropFileName)
import           System.Directory            (createDirectoryIfMissing)

-- | Creates a new unique file handle and touches the file.
createNewFile :: TaskUUID -> JobUUID -> String -> IO FilePath
createNewFile taskUUID jobUUID ext = do
  let path = genNewFileName taskUUID jobUUID ext
  touchFile path
  return path

genNewFileName :: TaskUUID -> JobUUID -> String -> FilePath
genNewFileName taskUUID jobUUID ext = "output" </> show taskUUID </> show jobUUID <.> ext

touchFile :: FilePath -> IO ()
touchFile path = do
  let directory = dropFileName path
  createDirectoryIfMissing True directory
  writeFile path ""
