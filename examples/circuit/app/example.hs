module Main where

import Pipeline
import Prelude hiding (id, (<>), replicate)
import Data.Time.Format.ISO8601 (iso8601ParseM)

top10Task :: (ToRecord a, FromRecord a) => FilePath -> Circuit '[CSVStore] '[[a]] '[CSVStore [a]] '[CSVStore] '[[a]] '[CSVStore [a]] N1
top10Task fname = functionTask f (CSVStore fname)
  where
    f :: [a] -> [a]
    f = take 10

aggArtistsTask :: Circuit '[CSVStore, CSVStore, CSVStore]
                          '[[(String, Int)], [(String, Int)], [(String, Int)]]
                          '[CSVStore [(String, Int)], CSVStore [(String, Int)], CSVStore [(String, Int)]]
                          '[CSVStore]
                          '[[(Int, String)]]
                          '[CSVStore [(Int, String)]]
                          N3
aggArtistsTask = multiInputTask f (CSVStore "aggArtists.csv")
  where
    f :: HList '[[(String, Int)], [(String, Int)], [(String, Int)]] -> [(Int, String)]
    f (HCons day1 (HCons day2 (HCons day3 HNil))) = undefined


aggSongsTask :: Circuit '[CSVStore, CSVStore, CSVStore]
                        '[[(String, Int)], [(String, Int)], [(String, Int)]]
                        '[CSVStore [(String, Int)], CSVStore [(String, Int)], CSVStore [(String, Int)]]
                        '[CSVStore]
                        '[[(Int, String)]]
                        '[CSVStore [(Int, String)]]
                        N3
aggSongsTask = multiInputTask f (CSVStore "aggSongs.csv")
  where
    f :: HList '[[(String, Int)], [(String, Int)], [(String, Int)]] -> [(Int, String)]
    f (HCons day1 (HCons day2 (HCons day3 HNil))) = undefined


pipeline :: Circuit '[CSVStore, CSVStore, CSVStore]
                    '[[(String, Int)], [(String, Int)], [(String, Int)]]
                    '[CSVStore [(String, Int)], CSVStore [(String, Int)], CSVStore [(String, Int)]]
                    '[CSVStore, CSVStore]
                    '[[(Int, String)], [(Int, String)]]
                    '[CSVStore [(Int, String)], CSVStore [(Int, String)]]
                    N3
pipeline = replicate <> replicate <> replicate
           <->
           id <> swap <> swap <> id
           <->
           id <> id <> swap <> id <> id
           <->
           aggArtistsTask <> aggSongsTask
           <->
           top10Task "top10Artists.csv" <> top10Task "top10Songs.csv"
           




main :: IO ()
main = print "hello"
