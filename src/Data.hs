{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data where

import Data.Aeson
import GHC.Generics (Generic)

data Creds = Creds {
  username :: String,
  userkey :: String,
  apikey :: String
} deriving (Generic, Show, ToJSON)

newtype TokenResponse = TokenResponse{token :: String}
  deriving (Generic, Show, FromJSON)