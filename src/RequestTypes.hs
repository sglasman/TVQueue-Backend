{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module RequestTypes where

import           Data.Aeson      (FromJSON)
import           Data.Text       (Text)

import           GHC.Generics    (Generic)

data CreateUserRequest = CreateUserRequest {
    email    :: Text,
    password :: Text
} deriving (Show, Generic, FromJSON)

data LoginRequest = LoginRequest {
    loginEmail    :: Text,
    loginPassword :: Text
} deriving (Generic, FromJSON)
