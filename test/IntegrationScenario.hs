{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IntegrationScenario where

import           TestApp                        ( TestOutApp
                                                , evalOutAppTest
                                                , inToTest
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Exception              ( catch )
import           System.Directory               ( removeFile )
import           Db                             ( doMigrateAll
                                                , UserSeries(..)
                                                , UserSeason(..)
                                                , User(..)
                                                , UserEpisode(..)
                                                , runDbAction
                                                , Unique(SeasonAndSeries)
                                                , SeasonId
                                                , EntityField(..)
                                                )
import           TestUtils                      ( ignoreException
                                                , dbGetOrThrowEmpty
                                                , sampleSeason1
                                                , sampleSeason2
                                                )
import           Data                           ( EpisodeResponse(..)
                                                , MyDay(..)
                                                , UserSeasonType(..)
                                                )
import           Data.Time.Calendar             ( addDays
                                                , fromGregorian
                                                , Day
                                                )
import           Control.Monad.State            ( modify )
import           App                            ( bridge )
import           Data.Map                       ( singleton )
import           TestBridge                     ( TestBridge(..)
                                                , putTestBridge
                                                )
import           Database.Persist               ( insert
                                                , insert_
                                                , getBy
                                                , entityKey
                                                , entityVal
                                                , get
                                                , count
                                                , Filter
                                                , selectFirst
                                                , (==.)
                                                )
import           OutboundController             ( addOrUpdateSeries )
import           Err                            ( emptyErr )
import           Util                           ( orFail )
import           InboundController              ( handleAddSeasonRequest
                                                , handleAddFutureSeasonsRequest
                                                , handleGetEpisodesRequest
                                                , handleMarkEpisodeWatchedRequest
                                                )
import           RequestTypes                   ( AddSeasonRequest(..)
                                                , AddFutureSeasonsRequest(..)
                                                , MarkEpisodeWatchedRequest(..)
                                                )
import           TVQResponseTypes               ( episodes
                                                , userWatchedDate
                                                )
import           Test.HUnit                     ( assertEqual )

setup :: TestOutApp ()
setup = liftIO (catch (removeFile "test.db") ignoreException) >> doMigrateAll

runAll :: IO ()
runAll = mapM_ runTest [testGetEpisodes, testNewSeasons]

runTest :: TestOutApp () -> IO ()
runTest = void . evalOutAppTest

testGetEpisodes :: TestOutApp ()
testGetEpisodes = do
  setup
  putTestBridge 1 ("", sampleSeason1 (fromGregorian 2020 1 1) 10)
  addOrUpdateSeries 1
  userId <- runDbAction . insert $ User "" ""
  inToTest . handleAddSeasonRequest userId $ AddSeasonRequest 1
                                                              1
                                                              OriginalAirdates
  inToTest
    . handleMarkEpisodeWatchedRequest userId
    $ MarkEpisodeWatchedRequest 1
    $ JustBase
    . MyDay
    $ fromGregorian 2020 1 1
  response <- inToTest $ handleGetEpisodesRequest userId
  liftIO $ assertEqual
    "The correct number of episodes come back in the get episodes response"
    10
    (length $ episodes response)
  liftIO $ assertEqual "An episode was successfully marked watched"
                       (Just . MyDay $ fromGregorian 2020 1 1)
                       (userWatchedDate . head $ episodes response)
  return ()

testNewSeasons :: TestOutApp ()
testNewSeasons = do
  setup
  putTestBridge 1 ("", sampleSeason1 (fromGregorian 2020 1 1) 3)
  addOrUpdateSeries 1
  season1Id :: SeasonId <-
    fmap entityKey . dbGetOrThrowEmpty . getBy $ SeasonAndSeries 1 1
  userId <- runDbAction . insert $ User "" ""
  inToTest . handleAddSeasonRequest userId $ AddSeasonRequest 1
                                                              1
                                                              OriginalAirdates
  userEpisodeCount <- runDbAction $ count ([] :: [Filter UserEpisode])
  liftIO $ assertEqual
    "User has the correct number of episodes when initially adding the series"
    3
    userEpisodeCount
  putTestBridge 1 ("", sampleSeason1 (fromGregorian 2020 1 2) 4)
  addOrUpdateSeries 1
  userEpisodeCount <- runDbAction $ count ([] :: [Filter UserEpisode])
  liftIO $ assertEqual
    "User has the correct number of episodes when a season is updated with a new episode and dates are changed"
    4
    userEpisodeCount
  putTestBridge 1 ("", sampleSeason1 (fromGregorian 2020 1 2) 3)
  addOrUpdateSeries 1
  userEpisodeCount <- runDbAction $ count ([] :: [Filter UserEpisode])
  liftIO $ assertEqual
    "User has the correct number of episodes when an episode is deleted from the season"
    3
    userEpisodeCount
  inToTest . handleAddFutureSeasonsRequest userId $ AddFutureSeasonsRequest
    1
    True
  putTestBridge
    1
    ("", sampleSeason1 (fromGregorian 2020 1 2) 4 ++ sampleSeason2)
  addOrUpdateSeries 1
  userEpisodeCount <- runDbAction $ count ([] :: [Filter UserEpisode])
  liftIO $ assertEqual
    "User has the correct number of episodes when a new season is added and the user is set to automatically add new seasons"
    8
    userEpisodeCount
  season2Id <- fmap entityKey . dbGetOrThrowEmpty . getBy $ SeasonAndSeries 2 1
  userSeason2 <- fmap entityVal . dbGetOrThrowEmpty $ selectFirst
    [UserSeasonSeasonId ==. season2Id]
    []
  liftIO $ assertEqual
    "User has the correct season type when the new season is a dump"
    (Custom (MyDay $ fromGregorian 2021 1 1) 7)
    (userSeasonUserSeasonType userSeason2)
  return ()
