{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TVQResponseTypes where

import           Data.Aeson.Types               ( ToJSON )
import           Db                             ( SeasonId
                                                , UserId
                                                )
import           GHC.Generics                   ( Generic )
import           Data                           ( MyDay(..)
                                                , SeriesType
                                                )
import qualified TVDBResponseTypes             as TVDB
import           Data.Time.Calendar             ( toGregorian )
import           Util                           ( firstOfTriple )

data CreateUserResponse = CreateUserResponse { userId :: UserId, token :: String }
  deriving (Show, Eq, Generic, ToJSON)

newtype LoginResponse = LoginResponse { token :: String }
  deriving (Show, Eq, Generic, ToJSON)

newtype GetEpisodesResponse = GetEpisodesResponse { episodes :: [EpisodeResponse] } deriving (Show, Eq, Generic, ToJSON)

data EpisodeResponse = EpisodeResponse {
  episodeId       :: Int,
  seriesId        :: Int,
  seriesName      :: String,
  seasonNumber    :: Int,
  userDate        :: Maybe MyDay,
  name            :: Maybe String,
  userWatchedDate :: Maybe MyDay
} deriving (Show, Eq, Generic, ToJSON)

newtype SearchResponse = SearchResponse { results :: [SearchResult] } deriving (Show, Eq, Generic, ToJSON)

data SearchResult = SearchResult {
  seriesId :: Int,
  seriesName :: String,
  seriesType :: Maybe SeriesType,
  overview :: Maybe String,
  year :: Maybe Int
} deriving (Show, Eq, Generic, ToJSON)

searchResultFromTvdb :: TVDB.SearchResult -> SearchResult
searchResultFromTvdb =
  SearchResult
    <$> TVDB.id
    <*> (TVDB.seriesName :: TVDB.SearchResult -> String)
    <*> TVDB.status
    <*> TVDB.overview
    <*> (fmap . fmap) (fromInteger . firstOfTriple . toGregorian . getDay)
                      (TVDB.firstAired :: TVDB.SearchResult -> Maybe MyDay)

searchResponseFromTvdb :: TVDB.SearchResponse -> SearchResponse
searchResponseFromTvdb =
  SearchResponse . map searchResultFromTvdb . TVDB.results
