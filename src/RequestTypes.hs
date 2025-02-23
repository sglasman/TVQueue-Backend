{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RequestTypes where

import           Data.Aeson                     ( FromJSON )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data                           ( UserSeasonType
                                                , MyDay
                                                )

data CreateUserRequest = CreateUserRequest {
    email    :: String,
    password :: String
} deriving (Show, Generic, FromJSON)

data LoginRequest = LoginRequest {
    email    :: String,
    password :: String
} deriving (Show, Generic, FromJSON)

data AddSeasonRequest = AddSeasonRequest {
    seriesId :: Int,
    seasonNumber :: Int,
    userSeasonType :: UserSeasonType
} deriving (Show, Generic, FromJSON)

data AddFutureSeasonsRequest = AddFutureSeasonsRequest { seasonId :: Int, addFutureSeasons :: Bool } deriving (Show, Generic, FromJSON)

data MarkEpisodeWatchedRequest = MarkEpisodeWatchedRequest { episodeId :: Int, watched :: Maybe MyDay } deriving (Show, Generic, FromJSON)

newtype SearchRequest = SearchRequest { searchTerm :: String } deriving (Show, Generic, FromJSON)
