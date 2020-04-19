{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module RequestTypes where

import           Data.Aeson                     ( FromJSON )
import           Data.Text                      ( Text )

import           GHC.Generics                   ( Generic )
import           Data                           ( UserSeasonType )

data CreateUserRequest = CreateUserRequest {
    createUserEmail    :: Text,
    createUserPassword :: Text
} deriving (Show, Generic, FromJSON)

data LoginRequest = LoginRequest {
    loginEmail    :: Text,
    loginPassword :: Text
} deriving (Show, Generic, FromJSON)

data AddSeasonRequest = AddSeasonRequest {
    seriesId :: Int,
    seasonNumber :: Int,
    userSeasonType :: UserSeasonType
} deriving (Show, Generic, FromJSON)
