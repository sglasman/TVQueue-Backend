{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module TVDBAuth where

import Data
import Secret
import Requests
import Control.Monad.Trans.State
import Data.Aeson
import Control.Monad.IO.Class
import Network.HTTP.Req
import Control.Monad (liftM)
import qualified Data.HashMap.Strict as M

runAuthenticated :: (MonadIO m, HttpMethod method) => Request a b method -> StateT String m a
runAuthenticated req = do
  token <- get
  
    
getToken :: StateT String IO ()
getToken = do
 res <- runReq defaultHttpConfig $ req POST
  loginUrl
  (ReqBodyJson Secret.creds)
  jsonResponse
  mempty
 case fromJSON $ responseBody res of
    Success tr -> put $ token tr
    _ -> pure ()

loginUrl :: Url 'Https
loginUrl = https "api.thetvdb.com" /: "login"