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
import           Servant                        ( Handler(..)
                                                , ServerError
                                                )
import Servant.Auth.Server (JWTSettings)
import Util (Pointed(..))

data OutAppState dbBackend = OutAppState {
  token :: String,
  outBackend    :: dbBackend
} deriving Show

instance (Pointed dbBackend) => Pointed (OutAppState dbBackend) where
  point = OutAppState "" point

instance (DbBackend dbBackend) => ProvidesDbBackend (OutAppState dbBackend) where
  provideBackend f = f . outBackend

data InAppState dbBackend = InAppState {
  jwtSettings :: Maybe JWTSettings,
  inBackend :: dbBackend
}

instance Show (InAppState dbBackend) where
  show = const "InAppState"

instance (Pointed dbBackend) => Pointed (InAppState dbBackend) where
  point = InAppState Nothing point

instance (DbBackend dbBackend) => ProvidesDbBackend (InAppState dbBackend) where
  provideBackend f = f . inBackend

newtype App err state a = App { runApp :: LoggingT (StateT state (ExceptT err IO)) a}

type DefaultOutApp a = App OutErr (OutAppState SqliteBackend) a
type DefaultInApp a = App ServerError (InAppState SqliteBackend) a

deriving instance Functor (App err state)
deriving instance Applicative (App err state)
deriving instance Monad (App err state)
deriving instance MonadIO (App err state)
deriving instance MonadState state (App err state)
deriving instance MonadError err (App err state)
deriving instance MonadLogger (App err state)

instance R.MonadHttp (App OutErr backend) where
  handleHttpException (R.VanillaHttpException (L.HttpExceptionRequest _ (L.StatusCodeException response outMessage)))
    = liftEither . Left $ OutErr
      (Just . statusCode $ L.responseStatus response)
      (B.toString outMessage)
  handleHttpException e = liftIO $ throwIO e

evalApp :: (Pointed state) => App err state a -> IO (Either err (a, state))
evalApp (App app) =
  runExceptT (runStateT (runStdoutLoggingT app) point)

evalState :: (Pointed state) => App err state a -> IO (Maybe state)
evalState app = (<$>) snd . either (const Nothing) Just <$> evalApp app

evalAppResult :: (Pointed state) => App err state a -> IO (Maybe a)
evalAppResult app = (<$>) fst . either (const Nothing) Just <$> evalApp app

