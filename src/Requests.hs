{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Aeson
import Data.String
import Err
import Network.HTTP.Req
import App (App)
import Control.Monad.Error.Class (liftEither)

data Request a b method =
  Request
    { body :: a
    , method :: method
    , url :: Url Https
    }

makeRequest ::
     (ToJSON a, FromJSON b, HttpMethod method, MonadIO m, HttpBodyAllowed (AllowsBody method) 'CanHaveBody)
  => Request a b method
  -> String
  -> App m b
makeRequest r token = do
  res <-
    runReq defaultHttpConfig $
    req
      (method r)
      (url r)
      (ReqBodyJson . body $ r)
      jsonResponse
      (header "Authorization" $ fromString $ "Bearer " ++ token)
  liftEither $
    let code = responseStatusCode res
     in if code >= 400
          then Left $ Err (Just code) ""
          else case fromJSON $ responseBody res of
                 Success b -> Right b
                 Error err -> Left $ Err Nothing err