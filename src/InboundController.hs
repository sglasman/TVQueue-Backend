{-# LANGUAGE OverloadedStrings #-}

module InboundController
  ( handleCreateUserRequest
  , handleLoginRequest
  , handleAddSeasonRequest
  )
where

import           RequestTypes
import           ResponseTypes
import           App
import           Db
import           Database.Persist               ( insertUnique
                                                , getBy
                                                , entityVal
                                                , entityKey
                                                )
import           Crypto.PasswordStore           ( makePassword
                                                , verifyPassword
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Text                      ( unpack )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( void )
import           Control.Monad.Except           ( throwError )
import           Data.Maybe                     ( isJust )
import           Servant                        ( err400
                                                , err401
                                                , errBody
                                                , NoContent(..)
                                                )

import           JWT                            ( generateJWT )
import           Servant                        ( NoContent )
import           Database.Persist               ( getBy )

handleCreateUserRequest :: CreateUserRequest -> DefaultInApp CreateUserResponse
handleCreateUserRequest req = do
  inserted <- insertUser req
  maybe (throwError (err400 { errBody = "User already exists" }))
        (return . CreateUserResponse)
        inserted

insertUser :: CreateUserRequest -> DefaultInApp (Maybe UserId)
insertUser (CreateUserRequest email pass) = do
  passHash <- liftIO $ makePassword (encodeUtf8 pass) 17
  runDbAction $ insertUnique (User email passHash)

handleLoginRequest :: LoginRequest -> DefaultInApp LoginResponse
handleLoginRequest (LoginRequest email pass) = do
  existingUser <- runDbAction . getBy $ UniqueEmail email
  let error = err401 { errBody = "Incorrect password" }
  maybe
    (throwError error)
    (\user ->
      if verifyPassword (encodeUtf8 pass) (userPasswordHash $ entityVal user)
        then fmap LoginResponse (generateJWT $ entityKey user)
        else throwError error
    )
    existingUser

handleAddSeasonRequest :: UserId -> AddSeasonRequest -> DefaultInApp NoContent
handleAddSeasonRequest userId (AddSeasonRequest seriesId seasonNumber userSeasonType)
  = do
    seasonId <- entityKey . runDbAction . getBy $ SeasonAndSeries seasonNumber
                                                                  seriesId

