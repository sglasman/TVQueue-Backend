{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module API
  ( startService
  )
where

import           Servant                        ( (:>)
                                                , (:<|>)(..)
                                                , ReqBody
                                                , JSON
                                                , PostCreated
                                                , Post
                                                , NoContent(..)
                                                , ServerT
                                                , Server
                                                , serve
                                                , hoistServer
                                                , Handler(..)
                                                , Proxy(..)
                                                )
import Servant.Auth.Server (JWT)
import           Control.Monad.Trans.Except
import           RequestTypes                   ( CreateUserRequest, LoginRequest )
import           App                            ( DefaultInApp
                                                , evalApp
                                                )
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import           InboundController
import Data.Text (Text, pack)

type UnauthAPI
  = "users" :> ReqBody '[JSON] CreateUserRequest :> PostCreated '[JSON] NoContent :<|>
    "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] String

type API = UnauthAPI

newtype ApiApp a = ApiApp { app :: DefaultInApp a }

deriving instance Functor ApiApp
deriving instance Applicative ApiApp
deriving instance Monad ApiApp

apiAppToHandler :: ApiApp a -> Handler a
apiAppToHandler = Handler . ExceptT . (fmap . fmap) fst . evalApp . app

proxy :: Proxy API
proxy = Proxy

appServerT :: ServerT API ApiApp
appServerT = (ApiApp . fmap (const NoContent) . handleCreateUserRequest) :<|>
             const (return "")

server :: Server API
server = hoistServer proxy apiAppToHandler appServerT

waiApp :: Wai.Application
waiApp = serve proxy server

startService :: IO ()
startService = Warp.run 8081 waiApp
