{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

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

class HasBody a where
  getBody :: a -> (forall r. (forall b . (HttpBody b) => b -> r) -> r)
    
instance ToJSON a => HasBody a where
  getBody = ReqBodyJson
  
instance HasBody NoReqBody NoReqBody where getBody = id

type RequestOK a b bo m method = (HttpBody bo, HasBody a bo, FromJSON b, HttpMethod method, MonadIO m, HttpBodyAllowed (AllowsBody method) 'CanHaveBody)

makeRequest :: RequestOK a b body m method
  => Request a b method
  -> String
  -> App m b
makeRequest r token = do
  res <-
    runReq defaultHttpConfig $
    req
      (method r)
      (url r)
      (getBody body)
      jsonResponse
      (header "Authorization" $ fromString $ "Bearer " ++ token)
  liftEither $
    let code = responseStatusCode res
     in if code >= 400
          then Left $ Err (Just code) ""
          else case fromJSON $ responseBody res of
                 Success b -> Right b
                 Error err -> Left $ Err Nothing err