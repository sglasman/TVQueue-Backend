{-# LANGUAGE ScopedTypeVariables #-}
module OutboundController where

import           App
import           Control.Monad                  ( unless
                                                , void
                                                , when
                                                , zipWithM_
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger           ( logDebug
                                                , logDebugN
                                                )
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
import           Control.Monad.State            ( gets )
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
                                                , UserId
                                                , UserEpisode(..)
                                                , UserSeason(..)
                                                , UserSeries(..)
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
import           OutApp
import           Util
import           TVDBBridge                     ( TVDBBridge(..) )
import qualified Control.Monad.State           as S
                                                ( get
                                                , gets
                                                )
import           ControllerCommons
import           DbBackend                      ( ProvidesDbBackend )
import           TVDBResponseTypes              ( SearchResult
                                                , EpisodeResponse(..)
                                                )

addOrUpdateSeries :: (TVDBBridge bridge) => Int -> DefaultBridgeApp bridge ()
addOrUpdateSeries seriesId = do
  maybeSeries <- runDbAction $ getSeries seriesId
  name <- maybe (getSeriesName seriesId) (return . Db.seriesName) maybeSeries
  when (isNothing maybeSeries) . void . runDbAction $ insert
    (Db.Series name seriesId)
  updateSeasonsFromSeries seriesId

updateSeasonsFromSeries
  :: (TVDBBridge bridge) => Int -> DefaultBridgeApp bridge ()
updateSeasonsFromSeries seriesId = do
  existingSeasonNumbers <- (fmap . fmap)
    (seasonNumber . entityVal)
    (runDbAction $ selectList [SeasonSeriesId ==. seriesId] [])
  episodes <- getEpisodes seriesId
  let seasonNumbers :: [Int] = catMaybes . nub $ map airedSeason episodes
  let newSeasonNumbers = filter (`notElem` existingSeasonNumbers) seasonNumbers
  let episodesForSeason n = filter (\ep -> airedSeason ep == Just n) episodes
  seasons :: [Season] <- liftIO $ mapM
    (\n ->
      Season n
        <$> seasonTypeOfEpisodes (episodesForSeason n)
        <*> return seriesId
    )
    seasonNumbers
  seasonEntities <- runDbActions $ map
    (\season -> repsertBy
      (SeasonAndSeries (seasonNumber season) (seasonSeriesId season))
      season
    )
    seasons
  handleNewSeasons seriesId $ filter
    (\season -> (seasonNumber . entityVal) season `elem` newSeasonNumbers)
    seasonEntities
  zipWithM_ updateEpisodesForSeasonIfRequired
            seasonEntities
            (map episodesForSeason seasonNumbers)

handleNewSeasons :: Int -> [Entity Season] -> DefaultBridgeApp bridge ()
handleNewSeasons seriesId seasons = do
  users :: [UserId] <-
    (fmap . fmap) (userSeriesUserId . entityVal) . runDbAction $ selectList
      [UserSeriesSeriesId ==. seriesId, UserSeriesAddFutureSeasons ==. True]
      []
  void
    .   runDbActions
    $   users
    >>= (\userId -> map (insertNewUserSeason userId) seasons)

insertNewUserSeason :: UserId -> Entity Season -> DbAction ()
insertNewUserSeason userId season = void . insertUnique $ UserSeason
  userId
  (entityKey season)
  (defaultUserSeasonType . seasonType $ entityVal season)

defaultUserSeasonType :: SeasonType -> UserSeasonType
defaultUserSeasonType Ongoing          = OriginalAirdates
defaultUserSeasonType Finished         = OriginalAirdates
defaultUserSeasonType (PastDump   day) = Custom day 7
defaultUserSeasonType (FutureDump day) = Custom day 7

seasonTypeOfEpisodes :: [EpisodeResponse] -> IO SeasonType
seasonTypeOfEpisodes = daysToSeasonType . map (fmap getDay . firstAired)

updateEpisodesForSeasonIfRequired
  :: (TVDBBridge bridge)
  => Entity Season
  -> [EpisodeResponse]
  -> DefaultBridgeApp bridge ()
updateEpisodesForSeasonIfRequired seasonEntity episodes = do
  maybeMatchingEpisodeEntities :: [Maybe (Entity Episode)] <- runDbActions
    $ map (getBy . UniqueEpisodeTvdbId . tvdbId) episodes
  logDebugN
    .  pack
    $  "Maybe matching episode entities: "
    ++ show maybeMatchingEpisodeEntities
  allEpisodeEntitiesForSeason :: [Entity Episode] <- runDbAction
    $ selectList [EpisodeSeasonId ==. entityKey seasonEntity] []
  let positionsWhereEpisodeChanged :: [Bool] = zipWith
        (didEpisodeChange seasonEntity)
        episodes
        maybeMatchingEpisodeEntities
  let changedEpisodes :: [EpisodeResponse] =
        map fst . filter snd $ zip episodes positionsWhereEpisodeChanged
  let changedEntities :: [Maybe (Entity Episode)] = map fst . filter snd $ zip
        maybeMatchingEpisodeEntities
        positionsWhereEpisodeChanged
  let deletedEpisodeIds =
        filter (`notElem` map tvdbId episodes)
          . map (episodeTvdbId . entityVal)
          $ allEpisodeEntitiesForSeason
  unless (null changedEpisodes) $ updateChangedEpisodesForSeason
    seasonEntity
    changedEpisodes
    changedEntities
  unless (null deletedEpisodeIds)
    $ updateDeletedEpisodesForSeason deletedEpisodeIds

updateChangedEpisodesForSeason
  :: ProvidesDbBackend state
  => Entity Season
  -> [EpisodeResponse]
  -> [Maybe (Entity Episode)]
  -> App err state ()
updateChangedEpisodesForSeason season eps ents = do
  userSeasons :: [UserSeason] <-
    (fmap . fmap) entityVal . runDbAction $ selectList
      [UserSeasonSeasonId ==. entityKey season]
      []
  void . runDbActions . concat $ zipWith
    (generateUpdatesForChangedEpisode season userSeasons)
    eps
    ents

updateDeletedEpisodesForSeason
  :: ProvidesDbBackend state => [Int] -> App err state ()
updateDeletedEpisodesForSeason ids =
  void
    .   runDbActions
    $   ids
    >>= (\id ->
          [ deleteWhere [EpisodeTvdbId ==. id]
          , deleteWhere [UserEpisodeEpisodeTvdbId ==. id]
          ]
        )


didEpisodeChange
  :: Entity Season -> EpisodeResponse -> Maybe (Entity Episode) -> Bool
didEpisodeChange season epRes = maybe
  True
  (\entity -> entityVal entity /= toDbEpisode epRes (entityKey season))

generateUpdatesForChangedEpisode
  :: Entity Season
  -> [UserSeason]
  -> EpisodeResponse
  -> Maybe (Entity Episode)
  -> [DbAction ()]
generateUpdatesForChangedEpisode season userSeasons episode = maybe
  ( (void . insert . toDbEpisode episode $ entityKey season)
  : map
      (\userSeason -> void . insert $ UserEpisode
        (userSeasonUserId userSeason)
        (tvdbId episode)
        Nothing
        (getUserDate (userSeasonUserSeasonType userSeason)
                     (airedEpisodeNumber episode)
                     (firstAired episode)
        )
      )
      userSeasons
  )
  (\ent ->
    replace (entityKey ent) (toDbEpisode episode $ entityKey season)
      : map
          (\userSeason -> updateWhere
            [UserEpisodeEpisodeTvdbId ==. tvdbId episode]
            [ UserEpisodeUserEpisodeDate =. getUserDate
                (userSeasonUserSeasonType userSeason)
                (airedEpisodeNumber episode)
                (firstAired episode)
            ]
          )
          userSeasons
  )

daysToSeasonType :: [Maybe Day] -> IO SeasonType
daysToSeasonType days =
  daysAndTodayToSeasonType days . utctDay <$> getCurrentTime

daysAndTodayToSeasonType :: [Maybe Day] -> Day -> SeasonType
daysAndTodayToSeasonType days today
  | null days
  = Ongoing
  | any isNothing days
  = Ongoing
  | length justDays > 3 && lastDay < today && length uniqueDays == 1
  = PastDump . MyDay $ head justDays
  | length justDays > 3 && length uniqueDays == 1
  = FutureDump . MyDay $ head justDays
  | lastDay < today
  = Finished
  | otherwise
  = Ongoing
 where
  justDays   = catMaybes days
  lastDay    = maximum justDays
  uniqueDays = nub days
