{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module TVDBAuth
  ( runAuthenticated
  )
where

import           App
import           Control.Monad                  ( liftM )
import           Control.Monad.Error.Class      ( catchError )
import           Control.Monad.IO.Class
import           Control.Monad.Logger           ( logDebugNS )
import           Control.Monad.State.Class      ( modify )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State      ( StateT )
import           Data
import           Data.Aeson
import qualified Data.HashMap.Strict           as M
import           Data.String                    ( fromString )
import           Err
import           Network.HTTP.Req
import           Requests
import           Secret

runAuthenticated
  :: (RequestOK a b m method) => Request a b method -> DefaultApp m b
runAuthenticated req = do
  let res = makeRequest req
  catchError
    res
    (\err ->
      if code err == Just 401 then getToken >> runAuthenticated req else res
    )

getToken :: (MonadIO m) => DefaultApp m ()
getToken = do
  res <- req POST loginUrl (ReqBodyJson creds) jsonResponse mempty
  case fromJSON $ responseBody res of
    Success tr -> modify (\appState -> appState { App.token = Data.token tr })
    _          -> return ()

loginUrl :: Url 'Https
loginUrl = https "api.thetvdb.com" /: "login"
