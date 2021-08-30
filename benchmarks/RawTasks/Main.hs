{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                  (forM, forM_, mzero)
import           Criterion.Main
import           Data.Csv
import           Data.List.Unique               (count_)
import qualified Data.Vector                    as V (fromList)
import           GHC.Generics
import           Pipeline
import           Pipeline.Internal.Core.UUID
import           Prelude                        hiding (id, replicate, (<>))


newtype Artist = Artist {artistName :: String} deriving (Eq, Show, Generic, Ord, NFData)

data Track = Track
  { artist      :: Artist
  , contentName :: String
  }
  deriving (Eq, Show, Generic, Ord, NFData)

data TrackCount = TrackCount
  { _track  :: Track
  , countTC :: Int
  }
  deriving (Eq, Show, Generic, Ord, NFData)
data ArtistCount = ArtistCount
  { _artist :: Artist
  , countAC :: Int
  }
  deriving (Eq, Show, Generic, Ord, NFData)


data Listen = Listen
  { appleIdNumber               :: Int
  , appleMusicSubscription      :: Bool
  , track                       :: Track
  , buildVersion                :: String
  , clientIPAddress             :: String
  , contentProvider             :: String
  , contentSpecificType         :: String
  , deviceIdentifier            :: String
  , endPositionInMilliseconds   :: Float
  , endReasonType               :: String
  , eventEndTimestamp           :: String
  , eventReasonHintType         :: String
  , eventReceivedTimestamp      :: String
  , eventStartTimestamp         :: String
  , eventType                   :: String
  , featureName                 :: String
  , genre                       :: String
  , itemType                    :: String
  , mediaDurationInMilliseconds :: Maybe Float
  , mediaType                   :: String
  , metricsBucketId             :: Maybe Float
  , metricsClientId             :: String
  , millisecondsSincePlay       :: Float
  , offline                     :: Bool
  , playDurationMilliseconds    :: Float
  , sourceType                  :: String
  , startPositionInMilliseconds :: Float
  , storeCountryName            :: String
  , utcOffsetInSeconds          :: Float
  }
  deriving (Generic, NFData, Eq)

instance FromNamedRecord Listen where
  parseNamedRecord r =
    Listen
      <$> r
      .:  "Apple Id Number"
      <*> r
      .:  "Apple Music Subscription"
      <*> (Track <$> (Artist <$> r .: "Artist Name") <*> r .: "Content Name")
      <*> r
      .:  "Build Version"
      <*> r
      .:  "Client IP Address"
      <*> r
      .:  "Content Provider"
      <*> r
      .:  "Content Specific Type"
      <*> r
      .:  "Device Identifier"
      <*> r
      .:  "End Position In Milliseconds"
      <*> r
      .:  "End Reason Type"
      <*> r
      .:  "Event End Timestamp"
      <*> r
      .:  "Event Reason Hint Type"
      <*> r
      .:  "Event Received Timestamp"
      <*> r
      .:  "Event Start Timestamp"
      <*> r
      .:  "Event Type"
      <*> r
      .:  "Feature Name"
      <*> r
      .:  "Genre"
      <*> r
      .:  "Item Type"
      <*> r
      .:  "Media Duration In Milliseconds"
      <*> r
      .:  "Media Type"
      <*> r
      .:  "Metrics Bucket Id"
      <*> r
      .:  "Metrics Client Id"
      <*> r
      .:  "Milliseconds Since Play"
      <*> r
      .:  "Offline"
      <*> r
      .:  "Play Duration Milliseconds"
      <*> r
      .:  "Source Type"
      <*> r
      .:  "Start Position In Milliseconds"
      <*> r
      .:  "Store Country Name"
      <*> r
      .:  "UTC Offset In Seconds"

instance ToNamedRecord Listen where
  toNamedRecord p = namedRecord
    [ "Apple Id Number" .= appleIdNumber p
    , "Apple Music Subscription" .= appleMusicSubscription p
    , "Artist Name" .= (artistName . artist . track) p
    , "Build Version" .= buildVersion p
    , "Client IP Address" .= clientIPAddress p
    , "Content Name" .= (contentName . track) p
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

instance ToNamedRecord ArtistCount where
  toNamedRecord (ArtistCount a n) = namedRecord ["Artist Name" .= artistName a, "Count" .= n]
instance FromNamedRecord ArtistCount where
  parseNamedRecord r = ArtistCount <$> (Artist <$> r .: "Artist Name") <*> r .: "Count"


instance DefaultOrdered ArtistCount where
  headerOrder a = V.fromList ["Artist Name", "Count"]

instance ToNamedRecord TrackCount where
  toNamedRecord (TrackCount t n) = namedRecord
    ["Artist Name" .= (artistName . artist) t, "Content Name" .= contentName t, "Count" .= n]
instance FromNamedRecord TrackCount where
  parseNamedRecord r =
    TrackCount
      <$> (Track <$> (Artist <$> r .: "Artist Name") <*> r .: "Content Name")
      <*> r
      .:  "Count"
instance DefaultOrdered TrackCount where
  headerOrder t = V.fromList ["Artist Name", "Content Name", "Count"]


instance FromField Bool where
  parseField s | s == "TRUE"  = pure True
               | s == "FALSE" = pure False
               | otherwise    = mzero

instance ToField Bool where
  toField True  = "TRUE"
  toField False = "FALSE"


aggArtists :: HList '[[Listen] , [Listen] , [Listen]] -> [ArtistCount]
aggArtists (HCons day1 (HCons day2 (HCons day3 HNil))) =
  (map (uncurry ArtistCount) . reverse . count_ . map (artist . track)) (day1 ++ day2 ++ day3)


aggSongs :: HList '[[Listen] , [Listen] , [Listen]] -> [TrackCount]
aggSongs (HCons day1 (HCons day2 (HCons day3 HNil))) =
  (map (uncurry TrackCount) . reverse . count_ . map track) (day1 ++ day2 ++ day3)


main :: IO ()
main = do

  r <- (fetch'
      (HCons'
        (NamedCSVStore "benchmarks/data/jan.csv" :: NamedCSVStore [Listen])
        (HCons' (NamedCSVStore "benchmarks/data/feb.csv" :: NamedCSVStore [Listen])
                (HCons' (NamedCSVStore "benchmarks/data/mar.csv" :: NamedCSVStore [Listen]) HNil')
        )
      )
    )

  defaultMain
    [ bgroup
        "raw tasks"
        [ bench "ignore" $ nf aggSongs r
        , bench "songAgg" $ nf aggSongs r
        , bench "artistAgg" $ nf aggArtists r
        ]
    ]
