module TVDBBridge where

import           RequestLibrary                 ( getEpisodesRequest
                                                , getSeriesRequest
                                                )
import           TVDBResponseTypes              ( GetEpisodesResponse(..)
                                                , GetSeriesResponse(..)
                                                , EpisodeResponse(..)
                                                , SearchResponse
                                                )
import           App                            ( DefaultBridgeApp )
import           Util                           ( Pointed(..) )

class TVDBBridge bridge where
  getEpisodes :: Int -> DefaultBridgeApp bridge [EpisodeResponse]
  getSeriesName :: Int -> DefaultBridgeApp bridge String
  executeSearch :: String -> DefaultBridgeApp bridge SearchResponse

