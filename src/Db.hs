{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Db where

import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Control.Monad.Logger           ( runStdoutLoggingT
                                                , MonadLogger
                                                , LoggingT
                                                )
import           Control.Monad.Trans.Reader
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Database.Persist               ( insert
                                                , insertBy
                                                , getBy
                                                , replace
                                                , PersistRecordBackend
                                                , Unique
                                                , Entity(..)
                                                , Key
                                                )
import           Database.Persist.Sql           ( runSqlConn
                                                , PersistFieldSql
                                                , Migration
                                                , runMigration
                                                )
import           Database.Persist.Sql.Types.Internal
                                                ( SqlBackend )
import           Database.Persist.Sqlite        ( withSqliteConn )
import           Database.Persist.TH
import           DbBackend
import           App
import qualified Control.Monad.State.Class     as S
import qualified Data
import           Data.Maybe                     ( isJust )
import qualified Data.ByteString.UTF8          as B

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
      deriving Eq
    UserSeason sql=user_season
      userId UserId
      seasonId SeasonId
      userSeasonType Data.UserSeasonType
      startDate Data.MyDay Maybe
      UniqueUserSeason userId seasonId
    UserEpisode sql=user_episode
      userId UserId
      episodeTvdbId Int
      watchedOn Data.MyDay Maybe
      userEpisodeDate Data.MyDay Maybe
      UniqueUserEpisode userId episodeTvdbId
    User
      email Text
      passwordHash B.ByteString
      UniqueEmail email
  |]

type DbAction b = ReaderT SqlBackend (LoggingT IO) b

runDbActionsWithBackend
  :: (DbBackend dbBackend, MonadIO m) => [DbAction b] -> dbBackend -> m [b]
runDbActionsWithBackend actions dbBackend =
  liftIO $ runStdoutLoggingT $ runBackend dbBackend $ runSqlConn
    (sequence actions)

runDbActions :: ProvidesDbBackend state => [DbAction b] -> App err state [b]
runDbActions actions =
  S.get >>= provideBackend (runDbActionsWithBackend actions)

runDbAction :: ProvidesDbBackend state => DbAction b -> App err state b
runDbAction action = head <$> runDbActions [action]

repsertBy
  :: PersistRecordBackend record SqlBackend
  => Unique record
  -> record
  -> DbAction (Entity record)
repsertBy unique record = do
  mExisting <- getBy unique
  case mExisting of
    Just entity -> replace (entityKey entity) record >> return entity
    Nothing     -> (`Entity` record) <$> insert record

getSeries :: Int -> DbAction (Maybe Series)
getSeries = (fmap . fmap) entityVal <$> getBy . UniqueSeriesTvdbId

toDbEpisode :: Data.EpisodeResponse -> SeasonId -> Episode
toDbEpisode episodeResponse = Episode
  (Data.airedEpisodeNumber episodeResponse)
  (Data.tvdbId episodeResponse)
  (fmap pack . Data.episodeName $ episodeResponse)
  (Data.firstAired episodeResponse)

doMigrateAll :: (ProvidesDbBackend state) => App err state ()
doMigrateAll = runDbAction $ runMigration migrateAll
