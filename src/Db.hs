{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db
 
 where

import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift             (MonadUnliftIO)
import           Control.Monad.Logger                (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Trans.Reader
import           Data.Text                           (Text, pack)
import           Database.Persist.Class              (insert, insertBy, getBy, replace, PersistRecordBackend, Unique)
import           Database.Persist.Sql                (runSqlConn, PersistFieldSql, Entity(..), Key, Migration, runMigration)
import           Database.Persist.Sql.Types.Internal (SqlBackend)
import           Database.Persist.Sqlite             (withSqliteConn)
import           Database.Persist.TH
import           DbBackend
import App
import qualified Control.Monad.State.Class as S
import qualified Data
import Data.Maybe (isJust)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Series sql=series
      name Text
      tvdbId Int
      UniqueSeriesTvdbId tvdbId
      deriving Show Read
    Season
      number Int
      type Data.SeasonType
      seriesId Int
      SeasonAndSeries number seriesId
      deriving Show
    Episode
      number Int Maybe
      tvdbId Int
      name Text Maybe
      airDate Data.MyDay Maybe
      seasonId SeasonId
      UniqueEpisodeTvdbId tvdbId
    UserSeason sql=user_season
      userId UserId
      seasonId SeasonId
      userSeasonType Data.UserSeasonType
      UniqueUserSeason userId seasonId
    User
  |]

type DbAction a b = a -> ReaderT SqlBackend (LoggingT IO) b

runDbActionsWithBackend :: (DbBackend dbBackend, MonadIO m) => [(DbAction a b, a)] -> dbBackend -> m [b]
runDbActionsWithBackend actionsWithInputs dbBackend = liftIO $ runStdoutLoggingT $ runBackend dbBackend $ runSqlConn (mapM (\ (action, a) -> action a) actionsWithInputs)

runDbActions :: (MonadIO m, DbBackend dbBackend) => [(DbAction a b, a)] -> App m dbBackend [b]
runDbActions actionsWithInputs = (dbBackend <$> S.get) >>= runDbActionsWithBackend actionsWithInputs

runDbAction :: (MonadIO m, DbBackend dbBackend) => DbAction a b -> a -> App m dbBackend b
runDbAction action input = head <$> runDbActions [(action, input)]

runInsertMany :: (MonadIO m
  , AtLeastOneUniqueKey record
  , PersistRecordBackend record SqlBackend
  , DbBackend dbBackend) =>
  [record] ->
  App m dbBackend [Entity record]
runInsertMany records = do
  eithers :: [Either (Entity record) (Key record)] <- runDbActions $ map ((,) insertBy) records
  return $ zipWith (\record e -> either id (flip Entity record) e) records eithers

repsertBy :: PersistRecordBackend record SqlBackend
          => Unique record
          -> DbAction record (Key record)
repsertBy unique record = do
  mExisting <- getBy unique
  case mExisting of
    Just (Entity key _) -> replace key record >> return key
    Nothing -> insert record

getSeries :: DbAction Int (Maybe Series)
getSeries = (fmap . fmap) entityVal <$> getBy . UniqueSeriesTvdbId

toDbEpisode :: Data.EpisodeResponse -> SeasonId -> Episode
toDbEpisode episodeResponse = Episode
  (Data.airedEpisodeNumber episodeResponse)
  (Data.tvdbId episodeResponse)
  (fmap pack . Data.episodeName $ episodeResponse)
  (Data.firstAired episodeResponse)
  
doMigrateAll :: MonadIO m => DefaultApp m ()
doMigrateAll = runDbAction runMigration migrateAll