{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module DbBackend where

import           Database.Persist.Class         ( PersistStoreWrite )
import           Data.Text                      ( Text )
import           Database.Persist.Sqlite        ( withSqliteConn )
import           Database.Persist.Sql.Types.Internal
                                                ( SqlBackend )
import           Control.Monad.Logger           ( runStdoutLoggingT
                                                , LoggingT
                                                , MonadLogger
                                                )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Util                           ( Pointed(..) )

class DbBackend dbBackend where
  runBackend :: (MonadUnliftIO m, MonadLogger m) => dbBackend -> (SqlBackend -> m a) -> m a

class ProvidesDbBackend a where
  provideBackend :: (forall backend . (DbBackend backend) => (backend -> b)) -> (a -> b)

newtype SqliteBackend = SqliteBackend { name :: Text } deriving Show
instance DbBackend SqliteBackend where
  runBackend (SqliteBackend name) = withSqliteConn name

defaultBackend :: SqliteBackend
defaultBackend = SqliteBackend "tvqbh_dev.db"

instance Pointed SqliteBackend where
  point = defaultBackend
