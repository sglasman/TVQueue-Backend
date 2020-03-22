{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module DbBackend where

import Database.Persist.Class (PersistStoreWrite)
import Data.Text (Text)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT, MonadLogger)
import Control.Monad.IO.Unlift (MonadUnliftIO)

class DbBackend dbBackend where
  runBackend :: forall a m . (MonadUnliftIO m, MonadLogger m) => dbBackend -> (SqlBackend -> m a) -> m a

newtype SqliteBackend = SqliteBackend { name :: Text } deriving Show
instance DbBackend SqliteBackend where
  runBackend (SqliteBackend name) = withSqliteConn name
  
defaultBackend :: SqliteBackend
defaultBackend = SqliteBackend "tvqbh_dev.db"