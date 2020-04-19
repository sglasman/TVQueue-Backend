{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JWT
  ( generateJWT
  )
where

import           App                            ( DefaultInApp
                                                , InAppState(..)
                                                )
import           Db                             ( UserId )
import           Control.Monad                  ( void )
import           Control.Monad.State            ( gets
                                                , modify
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )

import           Control.Monad.IO.Class         ( liftIO )
import           Servant.Auth.Server            ( JWTSettings
                                                , defaultJWTSettings
                                                , generateKey
                                                , makeJWT
                                                , ToJWT
                                                , FromJWT
                                                )
import           Data.ByteString.Lazy.UTF8      ( toString )

instance ToJWT UserId
instance FromJWT UserId

generateJWT :: UserId -> DefaultInApp String
generateJWT userId = do
  settings <- gets jwtSettings
  liftIO (makeJWT userId settings Nothing) >>= either
    (const $ refreshJWTSettings >> generateJWT userId)
    (return . toString)

refreshJWTSettings :: DefaultInApp JWTSettings
refreshJWTSettings = do
  key <- liftIO generateKey
  let settings = defaultJWTSettings key
  modify (\s -> s { jwtSettings = settings })
  return settings
