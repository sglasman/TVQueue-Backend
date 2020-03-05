{-# LANGUAGE OverloadedStrings #-}

module RequestLibrary where

import           Data
import           Data.String      (fromString)
import           Network.HTTP.Req
import           Requests

getEpisodesRequest :: Int -> Request NoReqBody GetEpisodesResponse GET
getEpisodesRequest seriesId = Request NoReqBody GET $ 
  https "api.thetvdb.com" /: "series" /: fromString (show seriesId) /: "episodes"