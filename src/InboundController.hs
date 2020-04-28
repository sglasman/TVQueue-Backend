{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InboundController
  ( handleCreateUserRequest
  , handleLoginRequest
  , handleAddSeasonRequest
  , handleAddFutureSeasonsRequest
  , handleGetEpisodesRequest
  )
where

import           App
import           Control.Monad                  ( void )
import           Control.Monad.Except           ( throwError )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.State            ( gets )
import           ControllerCommons
import           Crypto.PasswordStore           ( makePassword
                                                , verifyPassword
                                                )
import           Data.ByteString.Char8          ( pack )
import           Data.List                      ( nub )
import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                , maybe
                                                )
import           Data.Map                       ( Map
                                                , fromList
                                                , elems
                                                )
import qualified Data.Map                      as M
                                                ( map
                                                , lookup
                                                )
import           Database.Persist               ( Entity(..)
                                                , entityKey
                                                , entityVal
                                                , getBy
                                                , get
                                                , insertUnique
                                                , selectList
                                                , upsert
                                                , (=.)
                                                , (==.)
                                                )
import           Db
import           JWT                            ( generateJWT )
import           OutApp                         ( DefaultOutApp )
import           OutboundController             ( addOrUpdateSeries )
import           RequestTypes
import           ResponseTypes
import           Servant                        ( NoContent(..)
                                                , err400
                                                , err401
                                                , err404
                                                , errBody
                                                )
import           Util                           ( orFail )

handleCreateUserRequest :: CreateUserRequest -> DefaultInApp CreateUserResponse
handleCreateUserRequest req = do
  inserted <- insertUser req
  maybe (throwError (err400 { errBody = "User already exists" }))
        (return . CreateUserResponse)
        inserted

insertUser :: CreateUserRequest -> DefaultInApp (Maybe UserId)
insertUser (CreateUserRequest email pass) = do
  passHash <- liftIO $ makePassword (pack pass) 17
  runDbAction $ insertUnique (User email passHash)

handleLoginRequest :: LoginRequest -> DefaultInApp LoginResponse
handleLoginRequest (LoginRequest email pass) = do
  existingUser <- runDbAction . getBy $ UniqueEmail email
  let error = err401 { errBody = "Incorrect password" }
  maybe
    (throwError error)
    (\user -> if verifyPassword (pack pass) (userPasswordHash $ entityVal user)
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

handleAddFutureSeasonsRequest
  :: UserId -> AddFutureSeasonsRequest -> DefaultInApp NoContent
handleAddFutureSeasonsRequest userId (AddFutureSeasonsRequest seriesId add) =
  do
    runDbAction $ upsert (UserSeries userId seriesId add)
                         [UserSeriesAddFutureSeasons =. add]
    return NoContent

handleGetEpisodesRequest :: UserId -> DefaultInApp GetEpisodesResponse
handleGetEpisodesRequest userId = do
  userEps :: [UserEpisode] <- (fmap . fmap) entityVal . runDbAction $ selectList
    [UserEpisodeUserId ==. userId]
    []
  userEpsToEps :: Map UserEpisode Episode <- createMapFromUnique
    (UniqueEpisodeTvdbId . userEpisodeEpisodeTvdbId)
    userEps
  let seasonIds :: [SeasonId] = nub . map episodeSeasonId $ elems userEpsToEps
  seasonIdsToSeasons <- createMapFromKey id seasonIds
  let seriesIds :: [Int] = nub . map seasonSeriesId $ elems seasonIdsToSeasons
  seriesIdToSeries <- createMapFromUnique UniqueSeriesTvdbId seriesIds
  return . GetEpisodesResponse . catMaybes $ map
    (\userEp -> do
      ep     <- M.lookup userEp userEpsToEps
      season <- M.lookup (episodeSeasonId ep) seasonIdsToSeasons
      series <- M.lookup (seasonSeriesId season) seriesIdToSeries
      return $ EpisodeResponse (episodeTvdbId ep)
                               (seriesTvdbId series)
                               (Db.seriesName series)
                               (episodeSeasonId ep)
                               (userEpisodeUserEpisodeDate userEp)
                               (episodeName ep)
                               (userEpisodeWatchedOn userEp)
    )
    userEps

retrieveLocalSeason :: Int -> Int -> DefaultInApp (Maybe (Entity Season))
retrieveLocalSeason seasonNumber seriesId =
  runDbAction . getBy $ SeasonAndSeries seasonNumber seriesId

retrieveSeason :: Int -> Int -> DefaultInApp (Entity Season)
retrieveSeason seasonNumber seriesId = do
  maybeLocalSeason  <- retrieveLocalSeason seasonNumber seriesId
  maybeRemoteSeason <- maybe
    (do
      jwtSettings <- gets jwtSettings
      bridgeToIn (addOrUpdateSeries seriesId :: DefaultOutApp ())
      retrieveLocalSeason seasonNumber seriesId
    )
    (return . Just)
    maybeLocalSeason
  orFail err404 $ return maybeRemoteSeason
