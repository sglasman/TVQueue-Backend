{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Requests where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Control.Monad.Trans.State
import Data.String
import Control.Monad.Trans.Maybe

data Request a b method = Request { body :: a, method :: method, url :: Url Https }

runRequest :: (ToJSON a, FromJSON b, HttpMethod method) => Request a b method -> State String (MaybeT IO b)
runRequest r = do
  token :: String <- get
  res <- runReq defaultHttpConfig $ req
    (method r)
    (url r)
    (ReqBodyJson . body $ r)
    jsonResponse
    (header "Authorization" $ fromString $ "Bearer " ++ token)
  case fromJSON $ responseBody res of
    Success b -> Just b
    _ -> Nothing