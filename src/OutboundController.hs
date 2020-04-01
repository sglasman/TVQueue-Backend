{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module OutboundController where

import           App
import           Control.Monad          (void, when, zipWithM_, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger   (logDebug)
import           Data
import           Data.List              (nub)
import qualified Data.Map               as M
import           Data.Maybe             (catMaybes, isNothing, mapMaybe, maybe)
import           Data.Text              (pack, unpack)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Persist.Class (deleteWhere, get, getBy, insert,
                                         insertUnique, putMany, selectFirst,
                                         upsert, selectList, updateWhere)
import           Database.Persist.Sql   (Entity (..), (=.), (==.))
import           Db                     (DbAction, EntityField (..),
                                         Episode (..), Season (..), SeasonId,
                                         Unique (..), User (..),
                                         UserSeason (..), UserEpisode(..), getSeries, repsertBy,
                                         runDbAction, runDbActions, toDbEpisode)
import qualified Db                     (Episode (..), Series (..))
import           RequestLibrary
import           TVDBAuth
import           Util

getEpisodesCollated :: Int -> DefaultApp IO [EpisodeResponse]
getEpisodesCollated id = do
  firstPage <- runAuthenticated $ getEpisodesRequest id 1
  otherPages <- mapM (runAuthenticated . getEpisodesRequest id) [2..(pageCount firstPage)]
  return $ episodes firstPage ++ (otherPages >>= episodes)

addOrUpdateSeries :: Int -> DefaultApp IO ()
addOrUpdateSeries seriesId = do
  maybeSeries <- runDbAction $ getSeries seriesId
  name <- maybe (fmap seriesName $ runAuthenticated $ getSeriesRequest seriesId) (return . unpack . Db.seriesName) maybeSeries
  when (isNothing maybeSeries) . void . runDbAction $ insert (Db.Series (pack name) seriesId)
  updateSeasonsFromSeries seriesId

updateSeasonsFromSeries :: Int -> DefaultApp IO ()
updateSeasonsFromSeries seriesId = do
  episodes <- getEpisodesCollated seriesId
  let seasonNumbers :: [Int] = catMaybes . nub $ map airedSeason episodes
  let episodesForSeason n = filter (\ep -> airedSeason ep == Just n) episodes
  let seasonTypeOfSeason n = daysToSeasonType $ mapMaybe (fmap getDay . firstAired) (episodesForSeason n)
  seasons :: [Season] <- liftIO $ mapM (\n -> Season n <$> seasonTypeOfSeason n <*> return seriesId) seasonNumbers
  seasonKeys <- runDbActions $ map (\season -> repsertBy (SeasonAndSeries (seasonNumber season) (seasonSeriesId season)) season) seasons
  zipWithM_ updateEpisodesForSeasonIfRequired seasonKeys (map episodesForSeason seasonNumbers)

updateEpisodesForSeasonIfRequired :: SeasonId -> [EpisodeResponse] -> DefaultApp IO ()
updateEpisodesForSeasonIfRequired seasonId episodes = do
-- runDbAction (\_ -> deleteWhere [EpisodeSeasonId ==. seasonId]) ()
-- runDbActions $ map (\ep -> (insert, toDbEpisode ep seasonId)) episodes
  maybeEpisodeEntities :: [Maybe (Entity Episode)] <- runDbActions $ map (getBy . UniqueEpisodeTvdbId . tvdbId) episodes
  let positionsWhereEpisodeChanged :: [Bool] = zipWith (didEpisodeChange seasonId) episodes maybeEpisodeEntities
  let changedEpisodes :: [EpisodeResponse] = map fst . filter snd $ zip episodes positionsWhereEpisodeChanged
  let changedEntities :: [Maybe (Entity Episode)] = map fst . filter snd $ zip maybeEpisodeEntities positionsWhereEpisodeChanged
  unless (null changedEpisodes) $ updateEpisodesForSeason seasonId changedEpisodes changedEntities

updateEpisodesForSeason :: SeasonId -> [EpisodeResponse] -> [Maybe (Entity Episode)] -> DefaultApp IO ()
updateEpisodesForSeason seasonId eps ents = do
  userSeasons :: [UserSeason] <- (fmap . fmap) entityVal . runDbAction $ selectList [UserSeasonSeasonId ==. seasonId] []
  void . runDbActions . concat $ zipWith (createUpdates userSeasons) eps ents

didEpisodeChange :: SeasonId -> EpisodeResponse -> Maybe (Entity Episode) -> Bool
didEpisodeChange seasonId epRes = maybe True (\ entity -> entityVal entity /= toDbEpisode epRes seasonId)

didEpisodesChange :: SeasonId -> [EpisodeResponse] -> [Maybe (Entity Episode)] -> Bool
didEpisodesChange seasonId eps ents = or $ zipWith (didEpisodeChange seasonId) eps ents

createUpdates :: [UserSeason] -> EpisodeResponse -> Maybe (Entity Episode) -> [DbAction ()]
createUpdates userSeasons episode = maybe
  (map (\userSeason -> void . insert $ UserEpisode (userSeasonUserId userSeason) (tvdbId episode) Nothing (getUserDate userSeason episode)) userSeasons)
  (const [])
  
-- (\_ -> map (\userSeason -> updateWhere
--  [UserEpisodeEpisodeTvdbId ==. (tvdbId episode), UserEpisodeUserId ==. (userSeasonUserId userSeason)] 
--  [] 
--  ) userSeasons)
-- maybeEntity

getUserDate :: UserSeason -> EpisodeResponse -> Maybe MyDay
getUserDate _ _ = Nothing

daysToSeasonType :: [Day] -> IO SeasonType
daysToSeasonType days = daysAndTodayToSeasonType days . utctDay <$> getCurrentTime

daysAndTodayToSeasonType :: [Day] -> Day -> SeasonType
daysAndTodayToSeasonType days today
  | null days = Ongoing
  | length days > 3 && lastDay >= today && length uniqueDays == 1 = FutureDump
  | length days > 3 && length uniqueDays == 1 = PastDump
  | lastDay < today = Finished
  | otherwise = Ongoing
 where lastDay = maximum days
       uniqueDays = nub days
