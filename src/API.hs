{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module API
  ( startService,
    startServiceTls
  )
where

import           Servant                        ( (:>)
                                                , (:<|>)(..)
                                                , Context(..)
                                                , ReqBody
                                                , JSON
                                                , Get
                                                , PostCreated
                                                , Post
                                                , NoContent(..)
                                                , ServerT
                                                , Server
                                                , serveWithContext
                                                , hoistServerWithContext
                                                , Handler(..)
                                                , Proxy(..)
                                                , ServerError(..)
                                                , err401
                                                , QueryParam
                                                )
import           Servant.Auth.Server            ( FromJWT
                                                , ToJWT
                                                , JWT
                                                , Auth
                                                , AuthResult(..)
                                                , JWTSettings
                                                , CookieSettings
                                                , defaultCookieSettings
                                                , defaultJWTSettings
                                                , generateKey
                                                , throwAll
                                                )
import           Control.Monad.Trans.Except
import           RequestTypes                  
import           App                            ( DefaultInApp
                                                , evalAppFromState
                                                , evalAppResult
                                                , InAppState(..)
                                                , DefaultInAppState
                                                )
import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import           InboundController
import           TVQResponseTypes
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Db                             ( UserId )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Control.Monad.State            ( MonadState
                                                , gets
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson                     ( FromJSON(..) )
import           Util                           ( point )
import           Control.Monad.Logger           ( logDebugN
                                                , MonadLogger
                                                )

deriving instance Functor ApiApp
deriving instance Applicative ApiApp
deriving instance Monad ApiApp
deriving instance MonadError ServerError ApiApp
deriving instance MonadState DefaultInAppState ApiApp
deriving instance MonadIO ApiApp
deriving instance MonadLogger ApiApp

type UnauthAPI
  = "users" :> ReqBody '[JSON] CreateUserRequest :> PostCreated '[JSON] CreateUserResponse :<|>
    "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

type AuthAPI = Auth '[JWT] UserId :> (
                  "authtest" :> ReqBody '[JSON] LoginRequest :> Get '[JSON] NoContent :<|>
                  "seasons" :> (
                     "add" :> ReqBody '[JSON] AddSeasonRequest :> PostCreated '[JSON] NoContent :<|>
                     "addFuture" :> ReqBody '[JSON] AddFutureSeasonsRequest :> Post '[JSON] NoContent
                  ) :<|> 
                  "episodes" :> (Get '[JSON] GetEpisodesResponse :<|>
                     "watch" :> ReqBody '[JSON] MarkEpisodeWatchedRequest :> Post '[JSON] NoContent) :<|>
                  "search" :> QueryParam "searchterm" String :> Get '[JSON] SearchResponse)

type API = UnauthAPI :<|> AuthAPI

newtype ApiApp a = ApiApp { app :: DefaultInApp a }

instance FromJSON NoContent

apiAppToHandler :: JWTSettings -> ApiApp a -> Handler a
apiAppToHandler jwtSettings =
  Handler
    . ExceptT
    . (fmap . fmap) fst
    . evalAppFromState (InAppState jwtSettings point)
    . app

proxy :: Proxy API
proxy = Proxy

contextProxy :: Proxy '[CookieSettings, JWTSettings]
contextProxy = Proxy

unauthAppServerT :: ServerT UnauthAPI ApiApp
unauthAppServerT =
  (ApiApp . handleCreateUserRequest) :<|> (ApiApp . handleLoginRequest)

authAppServerT :: ServerT AuthAPI ApiApp
authAppServerT (Authenticated userId) = const (return NoContent) :<|>
                                        ((ApiApp . handleAddSeasonRequest userId) :<|>
                                        (ApiApp . handleAddFutureSeasonsRequest userId)) :<|>
                                        ((ApiApp $ handleGetEpisodesRequest userId) :<|>
                                        (ApiApp . handleMarkEpisodeWatchedRequest userId)) :<|>
                                        (ApiApp . handleSearchRequest userId)
                                      
authAppServerT _ = throwAll err401   

protect :: (UserId -> a -> ApiApp b) -> AuthResult UserId -> a -> ApiApp b
protect f (Authenticated userId) = f userId
protect _ authResult             = const $ throwError err401

appServerT :: ServerT API ApiApp
appServerT = unauthAppServerT :<|> authAppServerT

server :: JWTSettings -> Server API
server jwtSettings = hoistServerWithContext proxy
                                            contextProxy
                                            (apiAppToHandler jwtSettings)
                                            appServerT

waiApp :: IO Wai.Application
waiApp = do
  jwtSettings <- defaultJWTSettings <$> generateKey
  return $ serveWithContext
    proxy
    (defaultCookieSettings :. jwtSettings :. EmptyContext)
    (server jwtSettings)

startService :: IO ()
startService = waiApp >>= Warp.run 80

startServiceTls :: IO ()
startServiceTls = waiApp >>= runTLS (tlsSettings "cert.pem" "privkey.pem") (Warp.setPort 443 Warp.defaultSettings)