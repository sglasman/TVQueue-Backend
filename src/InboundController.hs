{-# LANGUAGE OverloadedStrings #-}

module InboundController (handleCreateUserRequest) where

import RequestTypes
import App
import Db
import Database.Persist (insertUnique)
import Crypto.PasswordStore (makePassword)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import Servant.Server.Internal.ServerError (err400, errBody)

handleCreateUserRequest :: CreateUserRequest -> DefaultInApp ()
handleCreateUserRequest req = do
  inserted <- insertUser req
  if inserted then return () else throwError (err400 {errBody = "User already exists"})

insertUser :: CreateUserRequest -> DefaultInApp Bool
insertUser (CreateUserRequest email pass) = do
  passHash <- liftIO $ makePassword (encodeUtf8 pass) 17
  fmap isJust . runDbAction $ insertUnique (User email passHash)