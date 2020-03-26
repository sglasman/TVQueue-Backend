{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module OutboundController where

import Data
import App
import TVDBAuth
import RequestLibrary
import Db (runDbAction, runDbActions, getSeries, runInsertMany, repsertBy, Season(..), Episode(..), SeasonId, EntityField(..), Unique(..), toDbEpisode, User(..), UserSeason(..))
import qualified Db (Series(..), Episode(..))
import Data.Maybe (maybe, isNothing, catMaybes, mapMaybe)
import Data.Text (unpack, pack)
import Control.Monad (when, void, zipWithM_)
import Database.Persist.Class (insert, insertUnique, putMany, upsert, deleteWhere, selectFirst)
import Database.Persist.Sql (Entity(..), (=.), (==.))
import Data.List (nub)
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import Util
import Control.Monad.Logger (logDebug)

getEpisodesCollated :: Int -> DefaultApp IO [EpisodeResponse]
getEpisodesCollated id = do
  firstPage <- runAuthenticated $ getEpisodesRequest id 1
  otherPages <- mapM (runAuthenticated . getEpisodesRequest id) [2..(pageCount firstPage)]
  return $ episodes firstPage ++ (otherPages >>= episodes)

addOrUpdateSeries :: Int -> DefaultApp IO ()
addOrUpdateSeries seriesId = do
  maybeSeries <- runDbAction getSeries seriesId
  name <- maybe (fmap seriesName $ runAuthenticated $ getSeriesRequest seriesId) (return . unpack . Db.seriesName) maybeSeries
  when (isNothing maybeSeries) $ void $ runDbAction insert (Db.Series (pack name) seriesId)
  updateSeasonsFromSeries seriesId

updateSeasonsFromSeries :: Int -> DefaultApp IO ()
updateSeasonsFromSeries seriesId = do
  episodes <- getEpisodesCollated seriesId
  let seasonNumbers :: [Int] = catMaybes . nub $ map airedSeason episodes
  let episodesForSeason n = filter (\ep -> airedSeason ep == Just n) episodes
  let seasonTypeOfSeason n = daysToSeasonType $ mapMaybe (fmap getDay . firstAired) (episodesForSeason n)
  seasons :: [Season] <- liftIO $ mapM (\n -> Season n <$> seasonTypeOfSeason n <*> return seriesId) seasonNumbers
  seasonKeys <- runDbActions (map (\season -> (repsertBy $ SeasonAndSeries (seasonNumber season) (seasonSeriesId season), season)) seasons)
  zipWithM_ updateEpisodesForSeason seasonKeys (map episodesForSeason seasonNumbers)

updateEpisodesForSeason :: SeasonId -> [EpisodeResponse] -> DefaultApp IO ()
updateEpisodesForSeason seasonId episodes = do
 runDbAction (\_ -> deleteWhere [EpisodeSeasonId ==. seasonId]) ()
 runDbActions $ map (\ep -> (insert, toDbEpisode ep seasonId)) episodes
 return ()

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

insertUserAndSeason :: DefaultApp IO ()
insertUserAndSeason = do
  userId <- runDbAction insert User
  maybeSeason :: Maybe (Entity Season) <- runDbAction (selectFirst []) []
  $logDebug . pack $ show maybeSeason
  maybe (return ()) (\season -> void $ runDbAction insert (UserSeason userId (entityKey season) OriginalAirdates)) maybeSeason