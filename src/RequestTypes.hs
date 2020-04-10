{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module RequestTypes where

import Data.Aeson (FromJSON)
import Data.Text (Text)

import GHC.Generics (Generic)

data CreateUserRequest = CreateUserRequest {
    username :: Text,
    password :: Text
} deriving (Generic, FromJSON)