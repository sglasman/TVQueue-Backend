{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module App where

import           Control.Exception.Base         ( throwIO )
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import qualified Data.ByteString.UTF8          as B
import           Database.Persist.Sql           ( SqlBackend )
import           DbBackend
import           Err
import qualified Network.HTTP.Client           as L
import           Network.HTTP.Req               ( handleHttpException )
import qualified Network.HTTP.Req              as R
import           Network.HTTP.Types.Status
import Servant (Handler(..), ServerError)

data AppState dbBackend = AppState {
  token :: String,
  dbBackend    :: dbBackend
} deriving Show

newtype App err backend a = App { runApp :: LoggingT (StateT (AppState backend) (ExceptT err IO)) a}

type DefaultApp err a = App err SqliteBackend a
type DefaultOutApp a = DefaultApp OutErr a
type DefaultInApp a = DefaultApp ServerError a

deriving instance Functor (App err backend)
deriving instance Applicative (App err backend)
deriving instance Monad (App err backend)
deriving instance MonadIO (App err backend)
deriving instance MonadState (AppState backend) (App err backend)
deriving instance MonadError err (App err backend)
deriving instance MonadLogger (App err backend)

instance R.MonadHttp (App OutErr backend) where
  handleHttpException (R.VanillaHttpException (L.HttpExceptionRequest _ (L.StatusCodeException response outMessage)))
    = liftEither . Left $ OutErr (Just . statusCode $ L.responseStatus response)
                              (B.toString outMessage)
  handleHttpException e = liftIO $ throwIO e

evalApp :: DefaultApp err a -> IO (Either err (a, AppState SqliteBackend))
evalApp (App app) =
  runExceptT (runStateT (runStdoutLoggingT app) (AppState "" defaultBackend))

evalState :: DefaultApp err a -> IO (Maybe (AppState SqliteBackend))
evalState app = (<$>) snd . either (const Nothing) Just <$> evalApp app

evalAppResult :: DefaultApp err a -> IO (Maybe a)
evalAppResult app = (<$>) fst . either (const Nothing) Just <$> evalApp app

