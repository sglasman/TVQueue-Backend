{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module App where

import           Control.Exception.Base     (throwIO)
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import qualified Data.ByteString.UTF8       as B
import           Database.Persist.Sql       (SqlBackend)
import           DbBackend
import           Err
import qualified Network.HTTP.Client        as L
import           Network.HTTP.Req           (handleHttpException)
import qualified Network.HTTP.Req           as R
import           Network.HTTP.Types.Status

newtype App m backend a = App { runApp :: LoggingT (StateT (AppState backend) (ExceptT Err m)) a}

type DefaultApp m a = App m SqliteBackend a

deriving instance Functor m => Functor (App m backend)
deriving instance Monad m => Applicative (App m backend)
deriving instance Monad m => Monad (App m backend)
deriving instance MonadIO m => MonadIO (App m backend)
deriving instance Monad m => MonadState (AppState backend) (App m backend)
deriving instance Monad m => MonadError Err (App m backend)
deriving instance MonadIO m => MonadLogger (App m backend)

instance MonadIO m => R.MonadHttp (App m backend) where
  handleHttpException (R.VanillaHttpException
    (L.HttpExceptionRequest _ (L.StatusCodeException response message))) =
      liftEither . Left $ Err (Just . statusCode $ L.responseStatus response) (B.toString message)
  handleHttpException e = liftIO $ throwIO e

evalApp :: DefaultApp IO a -> IO (Either Err (a, AppState SqliteBackend))
evalApp (App app) = runExceptT (runStateT (runStdoutLoggingT app) (AppState "" defaultBackend))

evalState :: DefaultApp IO a -> IO (Maybe (AppState SqliteBackend))
evalState app = (<$>) snd . either (const Nothing) Just <$> evalApp app

evalAppResult :: DefaultApp IO a -> IO (Maybe a)
evalAppResult app = (<$>) fst . either (const Nothing) Just <$> evalApp app

data AppState dbBackend = AppState {
  token :: String,
  dbBackend    :: dbBackend
} deriving Show
