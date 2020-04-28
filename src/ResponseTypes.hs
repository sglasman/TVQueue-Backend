{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ResponseTypes where

import           Data                           ( MyDay )
import           Data.Aeson.Types               ( ToJSON )
import           Db                             ( SeasonId
                                                , UserId
                                                )
import           GHC.Generics                   ( Generic )

newtype CreateUserResponse = CreateUserResponse { userId :: UserId }
  deriving (Show, Eq, Generic, ToJSON)

newtype LoginResponse = LoginResponse { token :: String }
  deriving (Show, Eq, Generic, ToJSON)

newtype GetEpisodesResponse = GetEpisodesResponse { episodes :: [EpisodeResponse] } deriving (Show, Eq, Generic, ToJSON)

data EpisodeResponse = EpisodeResponse {
  episodeId       :: Int,
  seriesId        :: Int,
  seriesName      :: String,
  seasonId        :: SeasonId,
  userDate        :: Maybe MyDay,
  name            :: Maybe String,
  userWatchedDate :: Maybe MyDay
} deriving (Show, Eq, Generic, ToJSON)
