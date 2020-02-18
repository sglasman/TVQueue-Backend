{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module TVDBAuth where

import Data
import Secret
import Control.Monad.Trans.Maybe
import Data.Aeson
import Control.Monad.IO.Class
import Network.HTTP.Req
import Control.Monad (liftM)
import qualified Data.HashMap.Strict as M

getToken :: MaybeT IO Value
getToken = do
 res <- runReq defaultHttpConfig $ req POST
  loginUrl
  (ReqBodyJson Secret.creds)
  jsonResponse
  mempty
 (MaybeT . return) $ case responseBody res of
  Object object -> M.lookup "token" object
  _ -> Nothing

loginUrl :: Url 'Https
loginUrl = https "api.thetvdb.com" /: "login"