{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module App where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Err
import Control.Monad.Error.Class
import Control.Monad.Trans.Maybe
import Network.HTTP.Req (MonadHttp, handleHttpException, HttpException(VanillaHttpException))
import qualified Network.HTTP.Client as L
import Control.Exception (throwIO)

newtype App m a = App { runApp :: StateT String (ExceptT Err m) a}

deriving instance Functor m => Functor (App m)
deriving instance Monad m => Applicative (App m)
deriving instance Monad m => Monad (App m)
deriving instance MonadIO m => MonadIO (App m)
deriving instance Monad m => MonadState String (App m)
deriving instance Monad m => MonadError Err (App m)

instance MonadIO m => MonadHttp (App m) where
  handleHttpException = liftIO . throwIO

evalApp :: App IO a -> IO (Either Err (a, String))
evalApp (App stateT) = runExceptT (runStateT stateT "")

evalToken :: App IO a -> IO (Maybe String)
evalToken app = (<$>) snd . either (const Nothing) Just <$> evalApp app

evalAppResult :: App IO a -> IO (Maybe a)
evalAppResult app = (<$>) fst . either (const Nothing) Just <$> evalApp app