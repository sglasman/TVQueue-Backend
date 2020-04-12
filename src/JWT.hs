module JWT
  ( generateJWT
  )
where

import           App                            ( DefaultInApp
                                                , InAppState(..)
                                                )
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
                                                )
import           Data.ByteString.Lazy.UTF8      ( toString )

instance ToJWT Text

generateJWT :: Text -> DefaultInApp String
generateJWT email = do
  maybeSettings <- gets jwtSettings
  settings      <- maybe refreshJWTSettings return maybeSettings
  liftIO (makeJWT email settings Nothing) >>= either
    (const $ refreshJWTSettings >> generateJWT email)
    (return . toString)

refreshJWTSettings :: DefaultInApp JWTSettings
refreshJWTSettings = do
  key <- liftIO generateKey
  let settings = defaultJWTSettings key
  modify (\s -> s { jwtSettings = Just settings })
  return settings
