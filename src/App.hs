{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module App where

import           Control.Exception.Base         ( throwIO )
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State            ( MonadState
                                                , put
                                                , gets
                                                , modify
                                                )
import           Control.Monad.Trans.State      ( StateT
                                                , runStateT
                                                )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.UTF8          as B
import qualified Data.ByteString.Lazy.Char8    as BSL
                                                ( pack )
import           Database.Persist.Sql           ( SqlBackend )
import           DbBackend
import           Err
import qualified Network.HTTP.Client           as L
import           Network.HTTP.Req               ( handleHttpException )
import qualified Network.HTTP.Req              as R
import           Network.HTTP.Types.Status
import           Servant                        ( Handler(..)
                                                , ServerError
                                                , err502
                                                , errBody
                                                )
import           Servant.Auth.Server            ( JWTSettings
                                                , defaultJWTSettings
                                                , generateKey
                                                )
import           Util                           ( Pointed(..) )

data BridgeAppState bridge dbBackend = BridgeAppState {
  bridge :: bridge,
  outBackend    :: dbBackend
} deriving Show

instance (Pointed bridge, Pointed dbBackend) => Pointed (BridgeAppState bridge dbBackend) where
  point = BridgeAppState point point

instance (DbBackend dbBackend) => ProvidesDbBackend (BridgeAppState bridge dbBackend) where
  provideBackend f = f . outBackend

data InAppState dbBackend = InAppState {
  jwtSettings :: JWTSettings,
  inBackend :: dbBackend
}

type DefaultInAppState = InAppState SqliteBackend

instance Show (InAppState dbBackend) where
  show = const "InAppState"

instance (DbBackend dbBackend) => ProvidesDbBackend (InAppState dbBackend) where
  provideBackend f = f . inBackend

newtype App err state a = App { runApp :: LoggingT (StateT state (ExceptT err IO)) a}

type DefaultBridgeApp bridge a
  = App OutErr (BridgeAppState bridge SqliteBackend) a
type DefaultInApp a = App ServerError DefaultInAppState a

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

evalAppFromState :: state -> App err state a -> IO (Either err (a, state))
evalAppFromState state (App app) =
  runExceptT (runStateT (runStdoutLoggingT app) state)

evalApp :: (Pointed state) => App err state a -> IO (Either err (a, state))
evalApp = evalAppFromState point

getMaybeState :: Either err (a, state) -> Maybe state
getMaybeState = (<$>) snd . either (const Nothing) Just

evalState :: (Pointed state) => App err state a -> IO (Maybe state)
evalState app = getMaybeState <$> evalApp app

getMaybeResult :: Either err (a, state) -> Maybe a
getMaybeResult = (<$>) fst . either (const Nothing) Just

evalAppResult :: (Pointed state) => App err state a -> IO (Maybe a)
evalAppResult app = getMaybeResult <$> evalApp app

fmapOtherArgs :: state0
  -> (err0 -> err1)
  -> App err0 state0 a
  -> App err1 state1 a
fmapOtherArgs state f app1 = do
  evald <- liftIO $ evalAppFromState state app1
  case evald of
    Left  err        -> liftEither . Left $ f err
    Right (a, state) -> return a

errToServerError :: OutErr -> ServerError
errToServerError (OutErr outCode outMessage) = err502
  { errBody = BSL.pack $ "Outbound error: " ++ show outCode ++ outMessage
  }

bridgeToIn :: Pointed bridge => DefaultBridgeApp bridge a -> DefaultInApp a
bridgeToIn = fmapOtherArgs point errToServerError
