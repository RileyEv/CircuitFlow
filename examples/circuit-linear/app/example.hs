{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Csv
import           Data.List.Unique           (count_)
import qualified Data.Vector                as V (fromList)
import           GHC.Generics
import           Pipeline
import           Prelude                    hiding (id, replicate, (<>))
import           System.Clock
import           Text.Printf

import           Control.Monad              (forM, forM_, mzero)
import           Control.Monad.Trans.Except (runExceptT)

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
  deriving (Generic, NFData)

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

top10Task
  :: (ToNamedRecord a, FromNamedRecord a, DefaultOrdered a, NFData a)
  => FilePath
  -> Circuit
       '[VariableStore]
       '[[a]]
       '[VariableStore [a]]
       '[NamedCSVStore]
       '[[a]]
       '[NamedCSVStore [a]]
       N1
top10Task fname = functionTask f (NamedCSVStore fname)
 where
  f :: [a] -> [a]
  f = take 10

aggArtistsTask
  :: Circuit
       '[NamedCSVStore , NamedCSVStore , NamedCSVStore]
       '[[Listen] , [Listen] , [Listen]]
       '[NamedCSVStore [Listen] , NamedCSVStore [Listen] , NamedCSVStore [Listen]]
       '[VariableStore]
       '[[ArtistCount]]
       '[VariableStore [ArtistCount]]
       N3
aggArtistsTask = multiInputTask f Empty
 where
  f :: HList '[[Listen] , [Listen] , [Listen]] -> [ArtistCount]
  f (HCons day1 (HCons day2 (HCons day3 HNil))) =
    (map (uncurry ArtistCount) . reverse . count_ . map (artist . track)) (day1 ++ day2 ++ day3)



aggSongsTask
  :: Circuit
       '[NamedCSVStore , NamedCSVStore , NamedCSVStore]
       '[[Listen] , [Listen] , [Listen]]
       '[NamedCSVStore [Listen] , NamedCSVStore [Listen] , NamedCSVStore [Listen]]
       '[VariableStore]
       '[[TrackCount]]
       '[VariableStore [TrackCount]]
       N3
aggSongsTask = multiInputTask f Empty
 where
  f :: HList '[[Listen] , [Listen] , [Listen]] -> [TrackCount]
  f (HCons day1 (HCons day2 (HCons day3 HNil))) =
    (map (uncurry TrackCount) . reverse . count_ . map track) (day1 ++ day2 ++ day3)


pipeline
  :: Circuit
       '[NamedCSVStore , NamedCSVStore , NamedCSVStore]
       '[[Listen] , [Listen] , [Listen]]
       '[NamedCSVStore [Listen] , NamedCSVStore [Listen] , NamedCSVStore [Listen]]
       '[NamedCSVStore , NamedCSVStore]
       '[[ArtistCount] , [TrackCount]]
       '[NamedCSVStore [ArtistCount] , NamedCSVStore [TrackCount]]
       N3
pipeline =
  replicate
    <>  replicate
    <>  replicate
    <-> id
    <>  swap
    <>  swap
    <>  id
    <-> id
    <>  id
    <>  swap
    <>  id
    <>  id
    <-> aggArtistsTask
    <>  aggSongsTask
    <-> top10Task "output/top10Artists.csv"
    <>  top10Task "output/top10Songs.csv"

addUser
  :: BasicNetwork
       '[NamedCSVStore , NamedCSVStore , NamedCSVStore]
       '[[Listen] , [Listen] , [Listen]]
       '[NamedCSVStore [Listen] , NamedCSVStore [Listen] , NamedCSVStore [Listen]]
       '[NamedCSVStore , NamedCSVStore]
       '[[ArtistCount] , [TrackCount]]
       '[NamedCSVStore [ArtistCount] , NamedCSVStore [TrackCount]]
  -> UUID
  -> IO ()
addUser n uuid = write
  uuid
  (HCons'
    (NamedCSVStore "../data/jan.csv")
    (HCons' (NamedCSVStore "../data/feb.csv") (HCons' (NamedCSVStore "../data/mar.csv") HNil'))
  )
  n

getUserTop10
  :: BasicNetwork
       '[NamedCSVStore , NamedCSVStore , NamedCSVStore]
       '[[Listen] , [Listen] , [Listen]]
       '[NamedCSVStore [Listen] , NamedCSVStore [Listen] , NamedCSVStore [Listen]]
       '[NamedCSVStore , NamedCSVStore]
       '[[ArtistCount] , [TrackCount]]
       '[NamedCSVStore [ArtistCount] , NamedCSVStore [TrackCount]]
  -> UUID
  -> IO (NamedCSVStore [ArtistCount], NamedCSVStore [TrackCount])
getUserTop10 n _ = do
  (Right (HCons' ac (HCons' tc HNil'))) <- output_ n
  return (ac, tc)


computeResult
  :: HList' '[NamedCSVStore , NamedCSVStore , NamedCSVStore] '[[Listen] , [Listen] , [Listen]]
  -> IO (NamedCSVStore [ArtistCount], NamedCSVStore [TrackCount])
computeResult inputs = do
  let (IIn7 (L (Task aggArtistsF aggArtistsEmpty))) = aggArtistsTask
  let (IIn7 (L (Task aggSongsF aggSongsEmpty)))     = aggSongsTask

  (Right aggArtists) <- runExceptT (aggArtistsF "test" inputs aggArtistsEmpty)
  (Right aggSongs  ) <- runExceptT (aggSongsF "test" inputs aggSongsEmpty)

  let artists = HCons' aggArtists HNil'
  let songs   = HCons' aggSongs HNil'
  let (IIn7 (L (Task top10ArtistsF top10ArtistsEmpty))) = top10Task "output/top10Artists.csv"
  let (IIn7 (L (Task top10SongsF top10SongsEmpty))) = top10Task "output/top10Songs.csv"

  (Right ac) <- runExceptT (top10ArtistsF "test" artists top10ArtistsEmpty)
  (Right tc) <- runExceptT (top10SongsF "test" songs top10SongsEmpty)

  return (ac, tc)


main :: IO ()
main = do
  let clock = Realtime
  startTime <- getTime clock
  let users = [ show x | x <- [0 .. 999] ]

  forM_
    users
    (const
      (computeResult
        (HCons'
          (NamedCSVStore "../data/jan.csv")
          (HCons' (NamedCSVStore "../data/feb.csv") (HCons' (NamedCSVStore "../data/mar.csv") HNil')
          )
        )
      )
    )


  endTime <- getTime clock

  let totalRunTime = fromIntegral (toNanoSecs (diffTimeSpec endTime startTime)) / (1e9 :: Double)
  printf "Total Runtime (s): %.6f\n" totalRunTime
