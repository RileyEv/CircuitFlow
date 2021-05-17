module Main where

import           BenchSongAgg.SongAgg (benchMain)
import           Criterion.Main



main :: IO ()
main = defaultMain
  [ bgroup
      "song aggregation"
      [ bench "1" $ nfIO (benchMain 1)
      , bench "10" $ nfIO (benchMain 10)
      , bench "100" $ nfIO (benchMain 100)
      -- , bench "11" $ nfIO (benchMain 1000)
      ]
  ]
