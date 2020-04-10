{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data
  ( Creds(..)
  , TokenResponse(..)
  , MyDay(..)
  , EpisodeResponse(..)
  , GetEpisodesResponse(..)
  , SeasonType(..)
  , GetSeriesResponse(..)
  , UserSeasonType(..)
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Aeson
import           Data.Text
import           Data.Time                      ( Day
                                                , defaultTimeLocale
                                                , parseTimeM
                                                )
import           Database.Persist.Sql           ( PersistField
                                                , PersistFieldSql
                                                , PersistValue(..)
                                                , toPersistValue
                                                )
import           Database.Persist.TH            ( derivePersistField )
import           GHC.Generics                   ( Generic )

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
  parseJSON v = withText
    ""
    (maybe (fail $ "Could not parse date " ++ show v) (pure . MyDay) . textToDay
    )
    v

timeFormatString :: String
timeFormatString = "%Y-%0m-%0d"

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
  parseJSON = withObject "" $ \o -> do
    airedEpisodeNumber <- o .:? "airedEpisodeNumber"
    airedSeason        <- o .:? "airedSeason"
    firstAired         <- o .:? "firstAired" <|> pure Nothing
    episodeName        <- o .:? "episodeName"
    tvdbId             <- o .: "id"
    return $ EpisodeResponse airedEpisodeNumber
                             airedSeason
                             firstAired
                             episodeName
                             tvdbId

data GetEpisodesResponse = GetEpisodesResponse {
  episodes  :: [EpisodeResponse],
  pageCount :: Int
} deriving (Show)

instance FromJSON GetEpisodesResponse where
  parseJSON = withObject
    ""
    (\o ->
      GetEpisodesResponse
        <$> (o .: "data")
        <*> (o .: "links" >>= flip (.:) "last")
    )

data SeasonType = Ongoing | Finished | PastDump | FutureDump deriving (Show, Read, Eq)
derivePersistField "SeasonType"

data UserSeasonType = OriginalAirdates | Custom { startDate :: MyDay, interval :: Int } deriving (Show, Read, Eq)
derivePersistField "UserSeasonType"

newtype GetSeriesResponse = GetSeriesResponse {
  seriesName :: String
} deriving (Show)

instance FromJSON GetSeriesResponse where
  parseJSON = withObject
    ""
    (\o -> GetSeriesResponse <$> (o .: "data" >>= flip (.:) "seriesName"))
   