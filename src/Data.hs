{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data where

import Data.Aeson
import GHC.Generics (Generic)
import Time.Types

data Creds = Creds {
  username :: String,
  userkey :: String,
  apikey :: String
} deriving (Generic, Show, ToJSON)

newtype TokenResponse = TokenResponse{token :: String}
  deriving (Generic, Show, FromJSON)

deriving instance Generic Month
deriving instance Generic Date

deriving instance FromJSON Month
deriving instance FromJSON Date

data EpisodeResponse = EpisodeResponse {
   airedEpisodeNumber :: Maybe Int,
   airedSeason :: Maybe Int,
   firstAired :: Maybe Date,
   episodeName :: Maybe String,
   id :: Int
} deriving (Generic, Show, FromJSON)

newtype GetEpisodesResponse = GetEpisodesResponse {
  episodes :: [EpisodeResponse]
} deriving (Show)

instance FromJSON GetEpisodesResponse where
  parseJSON = withObject "" (\o -> GetEpisodesResponse <$> o .: "data")