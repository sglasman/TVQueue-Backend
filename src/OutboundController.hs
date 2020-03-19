{-# LANGUAGE ScopedTypeVariables #-}
module OutboundController where

import Data
import App
import TVDBAuth
import RequestLibrary
import Db (runDbAction, getSeries, Season(..))
import qualified Db (Series(..))
import Data.Maybe (maybe, isNothing, catMaybes, mapMaybe)
import Data.Text (unpack, pack)
import Control.Monad (when, void)
import Database.Persist.Class (insert, insertUnique, putMany)
import Data.List (nub)
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import Util

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
  let episodesBySeason :: M.Map Int [EpisodeResponse] = mapToMap episodesForSeason seasonNumbers
  let seasonTypeOfSeason n = daysToSeasonType $ mapMaybe (fmap getDay . firstAired) (concat $ M.lookup n episodesBySeason)
  seasons :: [Season] <- liftIO $ mapM (\n -> Season n <$> seasonTypeOfSeason n <*> return seriesId) seasonNumbers
  runDbAction putMany seasons
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