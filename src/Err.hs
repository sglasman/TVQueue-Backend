module Err where

data Err =
  Err
    { code    :: Maybe Int
    , message :: String
    } deriving (Show)
