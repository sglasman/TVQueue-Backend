{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ResponseTypes where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON)

newtype LoginResponse = LoginResponse { token :: String }
  deriving (Show, Eq, Generic, ToJSON)