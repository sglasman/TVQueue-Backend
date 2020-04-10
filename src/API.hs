{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module API (startService) where

import Servant ((:>), (:<|>), ReqBody, JSON, PostCreated, NoContent(..), ServerT, Server, serve, hoistServer, Handler(..), Proxy(..))
import Control.Monad.Trans.Except
import RequestTypes (CreateUserRequest)
import App (DefaultInApp, evalApp)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import InboundController

type API = "users" :> ReqBody '[JSON] CreateUserRequest :> PostCreated '[JSON] NoContent

newtype ApiApp a = ApiApp { app :: DefaultInApp a }

deriving instance Functor ApiApp
deriving instance Applicative ApiApp
deriving instance Monad ApiApp

apiAppToHandler :: ApiApp a -> Handler a
apiAppToHandler = Handler . ExceptT . (fmap . fmap) fst . evalApp . app

proxy :: Proxy API
proxy = Proxy

appServerT :: ServerT API ApiApp
appServerT = ApiApp . fmap (const NoContent) . handleCreateUserRequest

server :: Server API
server = hoistServer proxy apiAppToHandler appServerT

waiApp :: Wai.Application
waiApp = serve proxy server

startService :: IO ()
startService = Warp.run 8081 waiApp