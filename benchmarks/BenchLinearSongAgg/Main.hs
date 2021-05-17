module Main where

import           BenchLinearSongAgg.SongAgg (benchMain)
import           Criterion.Main



main :: IO ()
main = defaultMain
  [ bgroup
      "song aggregation"
      [ bench "1" $ nfIO (benchMain 1)
      , bench "10" $ nfIO (benchMain 10)
      , bench "100" $ nfIO (benchMain 100)
      , bench "200" $ nfIO (benchMain 200)
      , bench "400" $ nfIO (benchMain 400)
      , bench "600" $ nfIO (benchMain 600)
      , bench "800" $ nfIO (benchMain 800)
      , bench "1000" $ nfIO (benchMain 1000)
      , bench "1200" $ nfIO (benchMain 1200)
      , bench "1400" $ nfIO (benchMain 1400)
      , bench "1600" $ nfIO (benchMain 1600)
      , bench "1800" $ nfIO (benchMain 1800)
      , bench "2000" $ nfIO (benchMain 2000)
      ]
  ]
