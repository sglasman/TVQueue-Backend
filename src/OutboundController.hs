{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module OutboundController where

import           App
import           Control.Monad                  ( unless
                                                , void
                                                , when
                                                , zipWithM_
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger           ( logDebug )
import           Data
import           Data.List                      ( nub )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , isNothing
                                                , mapMaybe
                                                , maybe
                                                )
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Persist.Class         ( deleteWhere
                                                , get
                                                , getBy
                                                , insert
                                                , insertUnique
                                                , putMany
                                                , replace
                                                , selectFirst
                                                , selectList
                                                , update
                                                , updateWhere
                                                , upsert
                                                )
import           Database.Persist.Sql           ( Entity(..)
                                                , (=.)
                                                , (==.)
                                                )
import           Db                             ( DbAction
                                                , EntityField(..)
                                                , Episode(..)
                                                , Season(..)
                                                , SeasonId
                                                , Unique(..)
                                                , User(..)
                                                , UserEpisode(..)
                                                , UserSeason(..)
                                                , getSeries
                                                , repsertBy
                                                , runDbAction
                                                , runDbActions
                                                , toDbEpisode
                                                )
import qualified Db                             ( Episode(..)
                                                , Series(..)
                                                )
import           RequestLibrary
import           TVDBAuth
import           Util

getEpisodesCollated :: Int -> DefaultOutApp [EpisodeResponse]
getEpisodesCollated id = do
  firstPage  <- runAuthenticated $ getEpisodesRequest id 1
  otherPages <- mapM (runAuthenticated . getEpisodesRequest id)
                     [2 .. (pageCount firstPage)]
  return $ episodes firstPage ++ (otherPages >>= episodes)

addOrUpdateSeries :: Int -> DefaultOutApp ()
addOrUpdateSeries seriesId = do
  maybeSeries <- runDbAction $ getSeries seriesId
  name        <- maybe
    (fmap seriesName $ runAuthenticated $ getSeriesRequest seriesId)
    (return . unpack . Db.seriesName)
    maybeSeries
  when (isNothing maybeSeries) . void . runDbAction $ insert
    (Db.Series (pack name) seriesId)
  updateSeasonsFromSeries seriesId

updateSeasonsFromSeries :: Int -> DefaultOutApp ()
updateSeasonsFromSeries seriesId = do
  episodes <- getEpisodesCollated seriesId
  let seasonNumbers :: [Int] = catMaybes . nub $ map airedSeason episodes
  let episodesForSeason n = filter (\ep -> airedSeason ep == Just n) episodes
  let seasonTypeOfSeason n = daysToSeasonType
        $ mapMaybe (fmap getDay . firstAired) (episodesForSeason n)
  seasons :: [Season] <- liftIO $ mapM
    (\n -> Season n <$> seasonTypeOfSeason n <*> return seriesId)
    seasonNumbers
  seasonEntities <- runDbActions $ map
    (\season -> repsertBy
      (SeasonAndSeries (seasonNumber season) (seasonSeriesId season))
      season
    )
    seasons
  zipWithM_ updateEpisodesForSeasonIfRequired
            seasonEntities
            (map episodesForSeason seasonNumbers)

updateEpisodesForSeasonIfRequired
  :: Entity Season -> [EpisodeResponse] -> DefaultOutApp ()
updateEpisodesForSeasonIfRequired seasonId episodes = do
  maybeEpisodeEntities :: [Maybe (Entity Episode)] <- runDbActions
    $ map (getBy . UniqueEpisodeTvdbId . tvdbId) episodes
  let positionsWhereEpisodeChanged :: [Bool] =
        zipWith (didEpisodeChange seasonId) episodes maybeEpisodeEntities
  let changedEpisodes :: [EpisodeResponse] =
        map fst . filter snd $ zip episodes positionsWhereEpisodeChanged
  let changedEntities :: [Maybe (Entity Episode)] = map fst . filter snd $ zip
        maybeEpisodeEntities
        positionsWhereEpisodeChanged
  unless (null changedEpisodes)
    $ updateEpisodesForSeason seasonId changedEpisodes changedEntities

updateEpisodesForSeason
  :: Entity Season
  -> [EpisodeResponse]
  -> [Maybe (Entity Episode)]
  -> DefaultOutApp ()
updateEpisodesForSeason season eps ents = do
  userSeasons :: [UserSeason] <-
    (fmap . fmap) entityVal . runDbAction $ selectList
      [UserSeasonSeasonId ==. entityKey season]
      []
  void . runDbActions . concat $ zipWith (createUpdates season userSeasons)
                                         eps
                                         ents

didEpisodeChange
  :: Entity Season -> EpisodeResponse -> Maybe (Entity Episode) -> Bool
didEpisodeChange season epRes = maybe
  True
  (\entity -> entityVal entity /= toDbEpisode epRes (entityKey season))

createUpdates
  :: Entity Season
  -> [UserSeason]
  -> EpisodeResponse
  -> Maybe (Entity Episode)
  -> [DbAction ()]
createUpdates season userSeasons episode = maybe
  (map
    (\userSeason -> void . insert $ UserEpisode
      (userSeasonUserId userSeason)
      (tvdbId episode)
      Nothing
      (getUserDate userSeason episode)
    )
    userSeasons
  )
  (\ent ->
    replace (entityKey ent) (toDbEpisode episode $ entityKey season)
      : map
          (\userSeason -> updateWhere
            [UserEpisodeEpisodeTvdbId ==. tvdbId episode]
            [UserEpisodeUserEpisodeDate =. getUserDate userSeason episode]
          )
          userSeasons
  )

getUserDate :: UserSeason -> EpisodeResponse -> Maybe MyDay
getUserDate userSeason episode = case userSeasonUserSeasonType userSeason of
  OriginalAirdates -> firstAired episode
  Custom (MyDay day) interval ->
    Just . MyDay $ addDays (toInteger interval) day

daysToSeasonType :: [Day] -> IO SeasonType
daysToSeasonType days =
  daysAndTodayToSeasonType days . utctDay <$> getCurrentTime

daysAndTodayToSeasonType :: [Day] -> Day -> SeasonType
daysAndTodayToSeasonType days today
  | null days       = Ongoing
  | length days > 3 && lastDay >= today && length uniqueDays == 1 = FutureDump
  | length days > 3 && length uniqueDays == 1 = PastDump
  | lastDay < today = Finished
  | otherwise       = Ongoing
 where
  lastDay    = maximum days
  uniqueDays = nub days