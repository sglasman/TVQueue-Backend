module Err where

data OutErr =
  OutErr
    { outCode :: Maybe Int
    , outMessage :: String
    } deriving (Show)
