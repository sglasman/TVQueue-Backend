{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module App where

import           Control.Exception.Base     (throwIO)
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import qualified Data.ByteString.UTF8       as B
import           Err
import qualified Network.HTTP.Client        as L
import           Network.HTTP.Req           (handleHttpException)
import qualified Network.HTTP.Req           as R
import           Network.HTTP.Types.Status

newtype App m a = App { runApp :: StateT String (ExceptT Err m) a}

deriving instance Functor m => Functor (App m)
deriving instance Monad m => Applicative (App m)
deriving instance Monad m => Monad (App m)
deriving instance MonadIO m => MonadIO (App m)
deriving instance Monad m => MonadState String (App m)
deriving instance Monad m => MonadError Err (App m)

instance MonadIO m => R.MonadHttp (App m) where
  handleHttpException (R.VanillaHttpException
    (L.HttpExceptionRequest _ (L.StatusCodeException response message))) =
      liftEither . Left $ Err (Just . statusCode $ L.responseStatus response) (B.toString message)
  handleHttpException e = liftIO $ throwIO e

evalApp :: App IO a -> IO (Either Err (a, String))
evalApp (App stateT) = runExceptT (runStateT stateT "")

evalToken :: App IO a -> IO (Maybe String)
evalToken app = (<$>) snd . either (const Nothing) Just <$> evalApp app

evalAppResult :: App IO a -> IO (Maybe a)
evalAppResult app = (<$>) fst . either (const Nothing) Just <$> evalApp app