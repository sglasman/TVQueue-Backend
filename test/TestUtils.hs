module TestUtils where

import Control.Exception (SomeException)
import DbBackend (ProvidesDbBackend)
import Database.Persist (Entity(..), Key)
import Db (DbAction, runDbAction)
import Util (orFail)
import App (App)
import Err (OutErr, emptyErr)

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

dbGetOrThrow :: (ProvidesDbBackend state) => err -> DbAction (Maybe (Entity a)) -> App err state (Entity a)
dbGetOrThrow err = orFail err . runDbAction

dbGetOrThrowEmpty :: (ProvidesDbBackend state) => DbAction (Maybe (Entity a)) -> App OutErr state (Entity a)
dbGetOrThrowEmpty = dbGetOrThrow emptyErr