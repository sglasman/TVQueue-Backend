{-# LANGUAGE ScopedTypeVariables #-}
module PollingService
  ( startPolling
  )
where

import           OutApp                         ( DefaultOutApp )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Concurrent             ( threadDelay )
import           Db                             ( runDbAction
                                                , Series
                                                , seriesTvdbId
                                                )
import           Database.Persist               ( selectList
                                                , Entity
                                                , entityVal
                                                )
import           Util                           ( interleave )
import           OutboundController             ( addOrUpdateSeries )

intervalBetweenPollsUs :: Int
intervalBetweenPollsUs = 1000 * 1000 * 60 * 60 * 6 -- 6 hours

intervalBetweenSeriesUs :: Int
intervalBetweenSeriesUs = 1000 * 1000 -- 1 second to avoid spamming the TVDB servers

startPolling :: DefaultOutApp ()
startPolling =
  sequence_ $ cycle [pollOnce, liftIO $ threadDelay intervalBetweenPollsUs]

pollOnce :: DefaultOutApp ()
pollOnce = do
  series :: [Entity Series] <- runDbAction $ selectList [] []
  sequence_ $ interleave
    (map (addOrUpdateSeries . seriesTvdbId . entityVal) series)
    (repeat . liftIO $ threadDelay intervalBetweenSeriesUs)
