{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data (
  Creds (..)
  , TokenResponse (..)
  , MyDay (..)
  , EpisodeResponse (..)
  , GetEpisodesResponse (..)
  , SeasonType (..)
  , GetSeriesResponse (..)
)where

import           Data.Aeson
import           Data.Text
import           GHC.Generics   (Generic)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Database.Persist.TH (derivePersistField)
import Database.Persist.Sql (PersistField, PersistFieldSql, toPersistValue, PersistValue(..))

data Creds = Creds {
  username :: String,
  userkey  :: String,
  apikey   :: String
} deriving (Generic, Show, ToJSON)

newtype TokenResponse = TokenResponse{token :: String}
  deriving (Generic, Show, FromJSON)

newtype MyDay = MyDay { getDay :: Day } deriving (Show, Read, Eq)
derivePersistField "MyDay"

instance FromJSON MyDay where
  parseJSON = withText "" $ maybe (fail "Could not parse date") (pure . MyDay) . textToDay

timeFormatString :: String
timeFormatString = "%Y-%m-%d"

textToDay :: Text -> Maybe Day
textToDay = parseTimeM True defaultTimeLocale timeFormatString . unpack

data EpisodeResponse = EpisodeResponse {
   airedEpisodeNumber :: Maybe Int,
   airedSeason        :: Maybe Int,
   firstAired         :: Maybe MyDay,
   episodeName        :: Maybe String,
   tvdbId             :: Int
} deriving (Generic, Show)

instance FromJSON EpisodeResponse where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = \s -> if s == "tvdbId" then "id" else s
    }

data GetEpisodesResponse = GetEpisodesResponse {
  episodes  :: [EpisodeResponse],
  pageCount :: Int
} deriving (Show)

instance FromJSON GetEpisodesResponse where
  parseJSON = withObject "" (\o ->
      GetEpisodesResponse <$> (o .: "data") <*> (o .: "links" >>= flip (.:) "last")
   )

data SeasonType = Ongoing | Finished | PastDump | FutureDump deriving (Show, Read, Eq)
derivePersistField "SeasonType"

newtype GetSeriesResponse = GetSeriesResponse {
  seriesName :: String
} deriving (Generic, FromJSON)