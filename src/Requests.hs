{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where

import           App                            ( App
                                                , BridgeAppState(..)
                                                , DefaultBridgeApp
                                                )

import           Control.Monad.Error.Class      ( liftEither )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.State.Class      ( get )
import           Data.Aeson
import           Data.Kind                      ( Constraint )
import           Data.String
import           Err
import           Network.HTTP.Req
import qualified Data.Text                     as T
import           DbBackend

data Request input output method =
  Request
    { input  :: input
    , method :: method
    , queryParams :: [(T.Text, String)]
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

type RequestOK input output method
  = ( RequestBody input
    , HttpBody (Body input)
    , FromJSON output
    , HttpMethod method
    , HttpBodyAllowed (AllowsBody method) (ProvidesBody (Body input))
    )

queryParamsToOption :: [(T.Text, String)] -> Option Https
queryParamsToOption = mconcat . map (uncurry (=:))
