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

module Db where

import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift             (MonadUnliftIO)
import           Control.Monad.Logger                (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Trans.Reader
import           Data.Text                           (Text)
import           Database.Persist.Class              (insert)
import           Database.Persist.Sql                (runSqlConn)
import           Database.Persist.Sql.Types.Internal (SqlBackend)
import           Database.Persist.Sqlite             (withSqliteConn)
import           Database.Persist.TH
import           DbBackend
import App

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Series sql=series
      name Text
      tvdbId Int
      UniqueTvdbId tvdbId
      deriving Show Read
  |]

type DbAction a b = a -> ReaderT SqlBackend (LoggingT IO) b

defaultBackend :: SqliteBackend
defaultBackend = SqliteBackend "tvqbh_dev.db"

runDbActionWithBackend :: (DbBackend dbBackend, MonadIO m) => dbBackend -> DbAction a b -> a -> m b
runDbActionWithBackend dbBackend action a = liftIO $ runStdoutLoggingT $ runBackend dbBackend (runSqlConn $ action a)

runDbAction :: (MonadIO m) => DbAction a b -> a -> m b
runDbAction = runDbActionWithBackend defaultBackend

insertSeries :: DbAction Series SeriesId
insertSeries = insert

addSeries :: Series -> App IO SeriesId
addSeries = runDbAction insertSeries