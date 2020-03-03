{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where

import           App                        (App)
import           Control.Monad.Error.Class  (liftEither)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Aeson
import           Data.Kind                  (Constraint)
import           Data.String
import           Err
import           Network.HTTP.Req

data Request input output method =
  Request
    { input  :: input
    , method :: method
    , url    :: Url Https
    }

class RequestBody a where
  type Body a 
  getBody :: a -> Body a
 
newtype JsonBody a = JsonBody a

instance ToJSON a => RequestBody (JsonBody a) where
  type Body (JsonBody a) = ReqBodyJson a
  getBody (JsonBody a) = ReqBodyJson a

instance RequestBody NoReqBody where
  type Body NoReqBody = NoReqBody
  getBody NoReqBody = NoReqBody

type RequestOK input output m method = (
  RequestBody input
  , HttpBody (Body input)
  , FromJSON output
  , HttpMethod method
  , MonadIO m
  , HttpBodyAllowed (AllowsBody method) (ProvidesBody (Body input))
  )

makeRequest :: RequestOK input output m method
  => Request input output method
  -> String
  -> App m output
makeRequest r token = do
  res <-
    runReq defaultHttpConfig $
    req
      (method r)
      (url r)
      (getBody $ input r)
      jsonResponse
      (header "Authorization" $ fromString $ "Bearer " ++ token)
  liftEither $
    let code = responseStatusCode res
     in if code >= 400
          then Left $ Err (Just code) ""
          else case fromJSON $ responseBody res of
                 Success b -> Right b
                 Error err -> Left $ Err Nothing err
