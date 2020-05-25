{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TVDBResponseTypes where

import           Control.Applicative            ( (<|>) )
import           Data                           ( SeriesType
                                                , MyDay
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , withObject
                                                , (.:)
                                                , (.:?)
                                                )
import           GHC.Generics                   ( Generic )

newtype SearchResponse = SearchResponse { results :: [SearchResult]}

instance FromJSON SearchResponse where
  parseJSON = withObject "" (\o -> SearchResponse <$> (o .: "data"))

data SearchResult = SearchResult {
  id         :: Int,
  seriesName :: String,
  status     :: Maybe SeriesType
} deriving (Show, Eq, Generic)

instance FromJSON SearchResult where
  parseJSON = withObject
    ""
    (\o -> do
      id         <- o .: "id"
      seriesName <- o .: "seriesName"
      status     <- Just <$> o .: "status" <|> return Nothing
      return $ SearchResult id seriesName status
    )

newtype GetSeriesResponse = GetSeriesResponse {
  seriesName :: String
} deriving (Show)

instance FromJSON GetSeriesResponse where
  parseJSON = withObject
    ""
    (\o -> GetSeriesResponse <$> (o .: "data" >>= flip (.:) "seriesName"))

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

newtype TokenResponse = TokenResponse{token :: String}
  deriving (Generic, Show, FromJSON)
