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

module Db where

import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift             (MonadUnliftIO)
import           Control.Monad.Logger                (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Trans.Reader
import           Data.Text                           (Text)
import           Database.Persist.Class              (insert, getBy)
import           Database.Persist.Sql                (runSqlConn, PersistFieldSql, Entity(..))
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
      seriesId SeriesId
    Episode
      number Int
      tvdbId Int
      name Text
      airDate Data.MyDay
      seasonId SeasonId
      UniqueEpisodeTvdbId tvdbId
  |]

type DbAction a b = a -> ReaderT SqlBackend (LoggingT IO) b

runDbActionWithBackend :: (DbBackend dbBackend, MonadIO m) => DbAction a b -> a -> dbBackend -> m b
runDbActionWithBackend action a dbBackend = liftIO $ runStdoutLoggingT $ runBackend dbBackend (runSqlConn $ action a)

runDbAction :: (MonadIO m, DbBackend dbBackend) => DbAction a b -> a -> App m dbBackend b
runDbAction action input = (dbBackend <$> S.get) >>= runDbActionWithBackend action input

insertSeries :: DbAction Series SeriesId
insertSeries = insert

addSeries :: Series -> DefaultApp IO SeriesId
addSeries = runDbAction insertSeries

getSeries :: DbAction Int (Maybe Series)
getSeries = (fmap . fmap) entityVal <$> getBy . UniqueSeriesTvdbId