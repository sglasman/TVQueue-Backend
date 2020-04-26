{-# LANGUAGE OverloadedStrings #-}

module InboundController
  ( handleCreateUserRequest
  , handleLoginRequest
  , handleAddSeasonRequest
  , handleAddFutureSeasonsRequest
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
                                                , selectList
                                                , (==.)
                                                , (=.)
                                                , upsert
                                                , Entity(..)
                                                )
import           Crypto.PasswordStore           ( makePassword
                                                , verifyPassword
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Text                      ( unpack )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( void )
import           Control.Monad.Except           ( throwError )
import           Control.Monad.State            ( gets )
import           Data.Maybe                     ( isJust
                                                , maybe
                                                )
import           Servant                        ( err400
                                                , err401
                                                , err404
                                                , errBody
                                                , NoContent(..)
                                                )
import           JWT                            ( generateJWT )
import           ControllerCommons
import           OutboundController             ( addOrUpdateSeries )
import           OutApp                         ( DefaultOutApp )
import Util (orFail)

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
    season <- retrieveSeason seasonNumber seriesId
    let seasonId = entityKey season
    episodes <- runDbAction $ selectList [EpisodeSeasonId ==. seasonId] []
    runDbAction $ insertUnique (UserSeries userId seriesId False)
    runDbAction $ repsertBy (UniqueUserSeason userId seasonId)
                            (UserSeason userId seasonId userSeasonType)
    runDbActions $ map
      (\(Entity _ (Episode number id _ airDate _)) ->
        let userEpisode = UserEpisode userId id Nothing
              $ getUserDate userSeasonType number airDate
        in  upsert
              userEpisode
              [ UserEpisodeWatchedOn =. userEpisodeWatchedOn userEpisode
              , UserEpisodeUserEpisodeDate
                =. userEpisodeUserEpisodeDate userEpisode
              ]
      )
      episodes
    return NoContent
    
handleAddFutureSeasonsRequest :: UserId -> AddFutureSeasonsRequest -> DefaultInApp NoContent
handleAddFutureSeasonsRequest userId (AddFutureSeasonsRequest seriesId add) = do
  runDbAction $ upsert (UserSeries userId seriesId add) [ UserSeriesAddFutureSeasons =. add ]
  return NoContent 

retrieveLocalSeason :: Int -> Int -> DefaultInApp (Maybe (Entity Season))
retrieveLocalSeason seasonNumber seriesId =
  runDbAction . getBy $ SeasonAndSeries seasonNumber seriesId

retrieveSeason :: Int -> Int -> DefaultInApp (Entity Season)
retrieveSeason seasonNumber seriesId = do
  maybeLocalSeason <- retrieveLocalSeason seasonNumber seriesId
  maybeRemoteSeason <- maybe
        (do
          jwtSettings <- gets jwtSettings
          bridgeToIn (addOrUpdateSeries seriesId :: DefaultOutApp ())
          retrieveLocalSeason seasonNumber seriesId
        )
        (return . Just)
        maybeLocalSeason
  orFail err404 $ return maybeRemoteSeason
