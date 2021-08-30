{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
module BenchLinearSongAgg.SongAgg where

import           Data.Csv
import           Data.List.Unique                          (count_)
import qualified Data.Vector                               as V (fromList)
import           GHC.Generics
import           Pipeline
import           Prelude                                   hiding (id,
                                                            replicate, (<>))
import           System.Clock
import           Text.Printf

import           Control.Monad                             (forM, forM_, mzero)
import           Control.Monad.Trans.Except                (runExceptT)

import           Pipeline.Internal.Common.IFunctor
import           Pipeline.Internal.Common.IFunctor.Modular
import           Pipeline.Internal.Core.CircuitAST
import           Pipeline.Internal.Core.UUID


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

top10Task
  :: (ToNamedRecord a, FromNamedRecord a, DefaultOrdered a, NFData a, Eq a)
  => Circuit
       '[Var]
       '[[a]]
       '[NamedCSVStore]
       '[[a]]
       N1
top10Task = functionTask f
 where
  f :: [a] -> [a]
  f = take 10

aggArtistsTask
  :: Circuit
       '[NamedCSVStore , NamedCSVStore , NamedCSVStore]
       '[[Listen] , [Listen] , [Listen]]
       '[Var]
       '[[ArtistCount]]
       N3
aggArtistsTask = multiInputTask f
 where
  f :: HList '[[Listen] , [Listen] , [Listen]] -> [ArtistCount]
  f (HCons day1 (HCons day2 (HCons day3 HNil))) =
    (map (uncurry ArtistCount) . reverse . count_ . map (artist . track)) (day1 ++ day2 ++ day3)



aggSongsTask
  :: Circuit
       '[NamedCSVStore , NamedCSVStore , NamedCSVStore]
       '[[Listen] , [Listen] , [Listen]]
       '[Var]
       '[[TrackCount]]
       N3
aggSongsTask = multiInputTask f
 where
  f :: HList '[[Listen] , [Listen] , [Listen]] -> [TrackCount]
  f (HCons day1 (HCons day2 (HCons day3 HNil))) =
    (map (uncurry TrackCount) . reverse . count_ . map track) (day1 ++ day2 ++ day3)


pipeline
  :: Circuit
       '[NamedCSVStore , NamedCSVStore , NamedCSVStore]
       '[[Listen] , [Listen] , [Listen]]
       '[NamedCSVStore , NamedCSVStore]
       '[[ArtistCount] , [TrackCount]]
       N3
pipeline = replicate2
    <>  replicate2
    <>  replicate2
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
    <-> top10Task
    <>  top10Task

addUser
  :: BasicNetwork
       '[NamedCSVStore , NamedCSVStore , NamedCSVStore]
       '[[Listen] , [Listen] , [Listen]]
       '[NamedCSVStore , NamedCSVStore]
       '[[ArtistCount] , [TrackCount]]
  -> JobUUID
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
       '[NamedCSVStore , NamedCSVStore]
       '[[ArtistCount] , [TrackCount]]
  -> JobUUID
  -> IO (NamedCSVStore [ArtistCount], NamedCSVStore [TrackCount])
getUserTop10 n _ = do
  (Right (HCons' ac (HCons' tc HNil'))) <- output_ n
  return (ac, tc)


computeResult
  :: HList' '[NamedCSVStore , NamedCSVStore , NamedCSVStore] '[[Listen] , [Listen] , [Listen]]
  -> JobUUID
  -> IO (NamedCSVStore [ArtistCount], NamedCSVStore [TrackCount])
computeResult inputs jobUUID = do
  let (IIn5 (R (R (R (R (R (R (R (L (Task aggArtistsF)))))))))) = aggArtistsTask
  let (IIn5 (R (R (R (R (R (R (R (L (Task aggSongsF))))))))))   = aggSongsTask
  aggATaskUUID <- genTaskUUID
  aggSTaskUUID <- genTaskUUID
  aggArtistsEmptyVar <- empty aggATaskUUID jobUUID
  aggSongsEmptyVar <- empty aggSTaskUUID jobUUID

  (Right aggArtists) <- runExceptT (aggArtistsF inputs aggArtistsEmptyVar)
  (Right aggSongs  ) <- runExceptT (aggSongsF inputs aggSongsEmptyVar)

  let artists = HCons' aggArtistsEmptyVar HNil'
  let songs   = HCons' aggSongsEmptyVar HNil'
  let (IIn5 (R (R (R (R (R (R (R (L (Task top10ArtistsF)))))))))) =
        top10Task
  let (IIn5 (R (R (R (R (R (R (R (L (Task top10SongsF)))))))))) =
        top10Task
  top10ATaskUUID <- genTaskUUID
  top10STaskUUID <- genTaskUUID
  top10ArtistsEmptyVar <- empty top10ATaskUUID jobUUID
  top10SongsEmptyVar <- empty top10STaskUUID jobUUID

  (Right ac) <- runExceptT (top10ArtistsF artists top10ArtistsEmptyVar)
  (Right tc) <- runExceptT (top10SongsF songs top10SongsEmptyVar)

  return (top10ArtistsEmptyVar, top10SongsEmptyVar)


benchMain :: Int -> IO ()
benchMain n = do
  users <- sequence [ genJobUUID | _ <- [0 .. (n - 1)] ]

  forM_
    users
    (computeResult
      (HCons'
        (NamedCSVStore "benchmarks/data/jan.csv")
        (HCons' (NamedCSVStore "benchmarks/data/feb.csv")
                (HCons' (NamedCSVStore "benchmarks/data/mar.csv") HNil')
        )
      )
    )
