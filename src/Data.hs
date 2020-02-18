{-# LANGUAGE DeriveGeneric #-}

module Data where

import Data.Aeson
import GHC.Generics (Generic)

data Creds = Creds {
  username :: String,
  userkey :: String,
  apikey :: String
} deriving (Generic, Show)

instance ToJSON Creds

newtype TokenResponse = TokenResponse{token :: String}
  deriving (Generic, Show)

instance FromJSON TokenResponse