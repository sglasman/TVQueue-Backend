{-# LANGUAGE OverloadedStrings #-}

module RequestLibrary where

import           Data
import           Data.String                    ( fromString )
import           Network.HTTP.Req
import           Requests

getSeriesRequest :: Int -> Request NoReqBody GetSeriesResponse GET
getSeriesRequest seriesId =
  Request NoReqBody GET [] $ https "api.thetvdb.com" /: "series" /: fromString
    (show seriesId)

getEpisodesRequest :: Int -> Int -> Request NoReqBody GetEpisodesResponse GET
getEpisodesRequest seriesId page =
  Request NoReqBody GET [("page", show page)]
    $  https "api.thetvdb.com"
    /: "series"
    /: fromString (show seriesId)
    /: "episodes"
