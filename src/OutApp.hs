{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module OutApp
  ( runAuthenticated
  , DefaultOutApp
  )
where

import           App
import           Control.Monad                  ( liftM )
import           Control.Monad.Except           ( liftEither
                                                , catchError
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Logger           ( logDebugN )
import           Control.Monad.State.Class      ( modify
                                                , gets
                                                )
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
import           TVDBBridge                     ( TVDBBridge(..) )
import           RequestLibrary                 ( getEpisodesRequest
                                                , getSeriesRequest
                                                , searchRequest
                                                )
import           Util                           ( Pointed(..) )
import qualified TVDBResponseTypes
import           TVDBResponseTypes              ( EpisodeResponse(..)
                                                , GetEpisodesResponse(..)
                                                , GetSeriesResponse(..)
                                                , SearchResponse(..)
                                                )
import           Data.Text                      ( pack )

newtype RealBridge = RealBridge { token :: String } deriving Show
instance Pointed RealBridge where
  point = RealBridge ""
instance TVDBBridge RealBridge where
  getEpisodes   = realGetEpisodes
  getSeriesName = realGetSeriesName
  executeSearch = realExecuteSearch

type DefaultOutApp a = DefaultBridgeApp RealBridge a

runAuthenticated
  :: (RequestOK a b method) => Request a b method -> DefaultOutApp b
runAuthenticated = goRunAuthenticated False

goRunAuthenticated
  :: (RequestOK a b method) => Bool -> Request a b method -> DefaultOutApp b
goRunAuthenticated alreadyTried req = do
  let res = makeRequest req
  catchError
    res
    (\err -> if outCode err == Just 401 && not alreadyTried
      then getToken >> goRunAuthenticated True req
      else res
    )

makeRequest
  :: (RequestOK input output method)
  => Request input output method
  -> DefaultOutApp output
makeRequest r = do
  token <- gets (OutApp.token . bridge)
  res   <- req
    (method r)
    (url r)
    (getBody $ input r)
    jsonResponse
    (  header "Authorization" (fromString $ "Bearer " ++ token)
    <> queryParamsToOption (queryParams r)
    )
  liftEither $ case fromJSON $ responseBody res of
    Success b   -> Right b
    Error   err -> Left $ OutErr Nothing err

getToken :: DefaultOutApp ()
getToken = do
  res <- req POST loginUrl (ReqBodyJson creds) jsonResponse mempty
  case fromJSON $ responseBody res of
    Success tr -> modify
      (\appState ->
        appState { bridge = RealBridge $ TVDBResponseTypes.token tr }
      )
    _ -> return ()

loginUrl :: Url 'Https
loginUrl = https "api.thetvdb.com" /: "login"

realGetEpisodes :: Int -> DefaultOutApp [EpisodeResponse]
realGetEpisodes id = do
  firstPage  <- runAuthenticated $ getEpisodesRequest id 1
  otherPages <- mapM (runAuthenticated . getEpisodesRequest id)
                     [2 .. (pageCount firstPage)]
  return $ episodes firstPage ++ (otherPages >>= episodes)

realGetSeriesName :: Int -> DefaultOutApp String
realGetSeriesName seriesId =
  fmap seriesName $ runAuthenticated $ getSeriesRequest seriesId

realExecuteSearch :: String -> DefaultOutApp SearchResponse
realExecuteSearch = runAuthenticated . searchRequest
