{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ResponseTypes where

import           GHC.Generics                   ( Generic )
import           Data.Aeson.Types               ( ToJSON )
import           Db                             ( UserId )

newtype CreateUserResponse = CreateUserResponse { userId :: UserId }
  deriving (Show, Eq, Generic, ToJSON)

newtype LoginResponse = LoginResponse { token :: String }
  deriving (Show, Eq, Generic, ToJSON)
