{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import Control.Monad.Trans.State
import Data.String
import Control.Monad.Trans.Maybe

data Request a b method = Request { body :: a, method :: method, url :: Url Https }

runRequest :: (ToJSON a, FromJSON b, HttpMethod method) => Request a b method -> MaybeT (StateT String IO) b
runRequest r = do
  token <- get
  res <- runReq defaultHttpConfig $ req
    (method r)
    (url r)
    (ReqBodyJson . body $ r)
    jsonResponse
    (header "Authorization" $ fromString $ "Bearer " ++ token)
  (MaybeT . return) $ case value of
    Success b -> Just b
    _ -> Nothing