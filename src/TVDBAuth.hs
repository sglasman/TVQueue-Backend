{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module TVDBAuth where

import Data
import Secret
import Requests
import Control.Monad.Trans.State (StateT)
import Data.Aeson
import Control.Monad.IO.Class
import Network.HTTP.Req
import Control.Monad (liftM)
import qualified Data.HashMap.Strict as M
import Err
import Control.Monad.Trans.Except
import App
import Control.Monad.Error.Class (catchError)
import Control.Monad.State.Class (get, put)

runAuthenticated :: (MonadIO m, HttpMethod method, ToJSON a, FromJSON b, HttpBodyAllowed (AllowsBody method) 'CanHaveBody) => Request a b method -> App m b
runAuthenticated req = do
  token <- get
  let res = makeRequest req token
  catchError res
    (\ err ->
       if code err == Just 401 then getToken >> runAuthenticated req else
         res)

getToken :: MonadIO m => App m ()
getToken = do
 res <- runReq defaultHttpConfig $ req POST
  loginUrl
  (ReqBodyJson Secret.creds)
  jsonResponse
  mempty
 case fromJSON $ responseBody res of
    Success tr -> put $ token tr
    _ -> return ()

loginUrl :: Url 'Https
loginUrl = https "api.thetvdb.com" /: "login"