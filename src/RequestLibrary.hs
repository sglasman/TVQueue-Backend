module RequestLibrary where

import Network.HTTP.Req (NoReqBody, GET)
import Data
import Requests

getEpisodesRequest :: Request NoReqBody GetEpisodesResponse GET