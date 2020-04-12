{-# LANGUAGE OverloadedStrings #-}

module InboundController
  ( handleCreateUserRequest
  , handleLoginRequest
  )
where

import           RequestTypes
import           App
import           Db
import           Database.Persist               ( insertUnique, getBy, entityVal )
import           Crypto.PasswordStore           ( makePassword, verifyPassword )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( void )
import           Control.Monad.Except           ( throwError )
import           Data.Maybe                     ( isJust )
import           Servant.Server.Internal.ServerError
                                                ( err400
                                                , err401
                                                , errBody
                                                )
import JWT (generateJWT)
                                               
handleCreateUserRequest :: CreateUserRequest -> DefaultInApp ()
handleCreateUserRequest req = do
  inserted <- insertUser req
  if inserted
    then return ()
    else throwError (err400 { errBody = "User already exists" })

insertUser :: CreateUserRequest -> DefaultInApp Bool
insertUser (CreateUserRequest email pass) = do
  passHash <- liftIO $ makePassword (encodeUtf8 pass) 17
  fmap isJust . runDbAction $ insertUnique (User email passHash)

handleLoginRequest :: LoginRequest -> DefaultInApp String
handleLoginRequest (LoginRequest email pass) = do
  existingUser <- runDbAction . getBy $ UniqueEmail email
  let error = err401 { errBody = "Incorrect password" }
  maybe
    (throwError error)
    (\user -> if verifyPassword (encodeUtf8 pass) (userPasswordHash $ entityVal user)
                then generateJWT email
                else throwError error)
    existingUser