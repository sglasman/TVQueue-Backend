{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IntegrationScenario where

import TestApp (TestOutApp, evalOutAppTest, inToTest)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch)
import System.Directory (removeFile)
import Db (doMigrateAll, UserSeries(..), UserSeason(..), User(..), UserEpisode(..), runDbAction, Unique(SeasonAndSeries), SeasonId, EntityField(..))
import TestUtils (ignoreException, dbGetOrThrowEmpty)
import Data (EpisodeResponse(..), MyDay(..), UserSeasonType(..))
import Data.Time.Calendar (addDays, fromGregorian, Day)
import Control.Monad.State (modify)
import App (bridge)
import Data.Map (singleton)
import TestBridge (TestBridge(..), putTestBridge)
import Database.Persist (insert, insert_, getBy, entityKey, entityVal, get, count, Filter, selectFirst, (==.))
import OutboundController (addOrUpdateSeries)
import Err (emptyErr)
import Util (orFail)
import InboundController (handleAddSeasonRequest, handleAddFutureSeasonsRequest)
import RequestTypes (AddSeasonRequest(..), AddFutureSeasonsRequest(..))
import Test.HUnit (assertEqual)

setup :: TestOutApp ()
setup = liftIO (
          catch (removeFile "test.db") ignoreException
          ) >> doMigrateAll

runTest :: TestOutApp () -> IO ()
runTest = void . evalOutAppTest 

testNewSeasons :: TestOutApp ()
testNewSeasons = do
  setup
  putTestBridge 1  ("", sampleSeason1 (fromGregorian 2020 1 1) 3)
  addOrUpdateSeries 1
  season1Id :: SeasonId <- fmap entityKey . dbGetOrThrowEmpty . getBy $ SeasonAndSeries 1 1
  userId <- runDbAction . insert $ User "" ""
  inToTest . handleAddSeasonRequest userId $ AddSeasonRequest 1 1 OriginalAirdates
  userEpisodeCount <- runDbAction $ count ([] :: [Filter UserEpisode])
  liftIO $ assertEqual "User has the correct number of episodes when initially adding the series" 3 userEpisodeCount
  putTestBridge 1 ("", sampleSeason1 (fromGregorian 2020 1 2) 4)
  addOrUpdateSeries 1
  userEpisodeCount <- runDbAction $ count ([] :: [Filter UserEpisode])
  liftIO $ assertEqual "User has the correct number of episodes when a season is updated with a new episode and dates are changed" 4 userEpisodeCount
  inToTest . handleAddFutureSeasonsRequest userId $ AddFutureSeasonsRequest 1 True
  putTestBridge 1 ("", sampleSeason1 (fromGregorian 2020 1 2) 4 ++ sampleSeason2)
  addOrUpdateSeries 1
  userEpisodeCount <- runDbAction $ count ([] :: [Filter UserEpisode])
  liftIO $ assertEqual "User has the correct number of episodes when a new season is added and the user is set to automatically add new seasons" 8 userEpisodeCount
  season2Id <- fmap entityKey . dbGetOrThrowEmpty . getBy $ SeasonAndSeries 2 1
  userSeason2 <- fmap entityVal . dbGetOrThrowEmpty $ selectFirst [UserSeasonSeasonId ==. season2Id] []
  liftIO $ assertEqual "User has the correct season type when the new season is a dump" (Custom (MyDay $ fromGregorian 2021 1 1) 7) (userSeasonUserSeasonType userSeason2)
  return ()

sampleSeason1 :: Day -> Int -> [EpisodeResponse]
sampleSeason1 startDate numberOfEps = map (\n -> EpisodeResponse (Just n) (Just 1) (Just . MyDay . (addDays . toInteger $ 7 * n) $ startDate) Nothing n) [1..numberOfEps]

sampleSeason2 :: [EpisodeResponse]
sampleSeason2 = map (\n -> EpisodeResponse (Just n) (Just 2) (Just . MyDay  $ fromGregorian 2021 1 1) Nothing (n + 100)) [1, 2, 3, 4]
