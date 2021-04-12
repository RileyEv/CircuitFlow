{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Pipeline
import Prelude hiding (id, (<>), replicate)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Csv
import GHC.Generics

import Control.Monad (mzero)

data Listen = Listen
  { appleIdNumber               :: Int
  , appleMusicSubscription      :: Bool
  , artistName                  :: String
  , buildVersion                :: String
  , clientIPAddress             :: String
  , contentName                 :: String
  , contentProvider             :: String
  , contentSpecificType         :: String
  , deviceIdentifier            :: String
  , endPositionInMilliseconds   :: Int
  , endReasonType               :: String
  , eventEndTimestamp           :: String  -- Change to UTCTime
  , eventReasonHintType         :: String
  , eventReceivedTimestamp      :: String  -- UTCTime
  , eventStartTimestamp         :: String  -- UTCTime
  , eventType                   :: String
  , featureName                 :: String
  , genre                       :: String
  , itemType                    :: String
  , mediaDurationInMilliseconds :: Int
  , mediaType                   :: String
  , metricsBucketId             :: Int
  , metricsClientId             :: String
  , millisecondsSincePlay       :: Int
  , offline                     :: Bool
  , playDurationMilliseconds    :: Int
  , sourceType                  :: String
  , startPositionInMilliseconds :: Int
  , storeCountryName            :: String
  , utcOffsetInSeconds          :: Int
  }
  deriving (Generic)

instance FromNamedRecord Listen where
  parseNamedRecord r = Listen <$> r .: "Apple Id Number"
                              <*> r .: "Apple Music Subscription"
                              <*> r .: "Artist Name"
                              <*> r .: "Build Version"
                              <*> r .: "Client IP Address"
                              <*> r .: "Content Name"
                              <*> r .: "Content Provider"
                              <*> r .: "Content Specific Type"
                              <*> r .: "Device Identifier"
                              <*> r .: "End Position In Milliseconds"
                              <*> r .: "End Reason Type"
                              <*> r .: "Event End Timestamp"
                              <*> r .: "Event Reason Hint Type"
                              <*> r .: "Event Received Timestamp"
                              <*> r .: "Event Start Timestamp"
                              <*> r .: "Event Type"
                              <*> r .: "Feature Name"
                              <*> r .: "Genre"
                              <*> r .: "Item Type"
                              <*> r .: "Media Duration In Milliseconds"
                              <*> r .: "Media Type"
                              <*> r .: "Metrics Bucket Id"
                              <*> r .: "Metrics Client Id"
                              <*> r .: "Milliseconds Since Play"
                              <*> r .: "Offline"
                              <*> r .: "Play Duration Milliseconds"
                              <*> r .: "Source Type"
                              <*> r .: "Start Position In Milliseconds"
                              <*> r .: "Store Country Name"
                              <*> r .: "UTC Offset In Seconds"

instance ToNamedRecord Listen where
  toNamedRecord p = namedRecord [ "Apple Id Number" .= appleIdNumber p
                                , "Apple Music Subscription" .= appleMusicSubscription p
                                , "Artist Name" .= artistName p
                                , "Build Version" .= buildVersion p
                                , "Client IP Address" .= clientIPAddress p
                                , "Content Name" .= contentName p
                                , "Content Provider" .= contentProvider p
                                , "Content Specific Type" .= contentSpecificType p
                                , "Device Identifier" .= deviceIdentifier p
                                , "End Position In Milliseconds" .= endPositionInMilliseconds p
                                , "End Reason Type" .= endReasonType p
                                , "Event End Timestamp" .= eventEndTimestamp p
                                , "Event Reason Hint Type" .= eventReasonHintType p
                                , "Event Received Timestamp" .= eventReceivedTimestamp p
                                , "Event Start Timestamp" .= eventStartTimestamp p
                                , "Event Type" .= eventType p
                                , "Feature Name" .= featureName p
                                , "Genre" .= genre p
                                , "Item Type" .= itemType p
                                , "Media Duration In Milliseconds" .= mediaDurationInMilliseconds p
                                , "Media Type" .= mediaType p
                                , "Metrics Bucket Id" .= metricsBucketId p
                                , "Metrics Client Id" .= metricsClientId p
                                , "Milliseconds Since Play" .= millisecondsSincePlay p
                                , "Offline" .= offline p
                                , "Play Duration Milliseconds" .= playDurationMilliseconds p
                                , "Source Type" .= sourceType p
                                , "Start Position In Milliseconds" .= startPositionInMilliseconds p
                                , "Store Country Name" .= storeCountryName p
                                , "UTC Offset In Seconds" .= utcOffsetInSeconds p
                                ]
instance DefaultOrdered Listen

instance FromField Bool where
  parseField s
    | s == "TRUE"  = pure True
    | s == "FALSE" = pure False
    | otherwise = mzero

instance ToField Bool where
  toField True  = "TRUE"
  toField False = "FALSE"

top10Task :: (ToRecord a, FromRecord a) => FilePath -> Circuit '[CSVStore] '[[a]] '[CSVStore [a]] '[CSVStore] '[[a]] '[CSVStore [a]] N1
top10Task fname = functionTask f (CSVStore fname)
  where
    f :: [a] -> [a]
    f = take 10

aggArtistsTask :: Circuit '[NamedCSVStore, NamedCSVStore, NamedCSVStore]
                          '[[Listen], [Listen], [Listen]]
                          '[NamedCSVStore [Listen], NamedCSVStore [Listen], NamedCSVStore [Listen]]
                          '[CSVStore]
                          '[[(Int, String)]]
                          '[CSVStore [(Int, String)]]
                          N3
aggArtistsTask = multiInputTask f (CSVStore "aggArtists.csv")
  where
    f :: HList '[[Listen], [Listen], [Listen]] -> [(Int, String)]
    -- Unique.count but with f
    f (HCons day1 (HCons day2 (HCons day3 HNil))) = undefined


aggSongsTask :: Circuit '[NamedCSVStore, NamedCSVStore, NamedCSVStore]
                        '[[Listen], [Listen], [Listen]]
                        '[NamedCSVStore [Listen], NamedCSVStore [Listen], NamedCSVStore [Listen]]
                        '[CSVStore]
                        '[[(Int, String)]]
                        '[CSVStore [(Int, String)]]
                        N3
aggSongsTask = multiInputTask f (CSVStore "aggSongs.csv")
  where
    f :: HList '[[Listen], [Listen], [Listen]] -> [(Int, String)]
    f (HCons day1 (HCons day2 (HCons day3 HNil))) = undefined


pipeline :: Circuit '[NamedCSVStore, NamedCSVStore, NamedCSVStore]
                    '[[Listen], [Listen], [Listen]]
                    '[NamedCSVStore [Listen], NamedCSVStore [Listen], NamedCSVStore [Listen]]
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
