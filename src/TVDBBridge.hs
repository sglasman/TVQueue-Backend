module TVDBBridge where

import           RequestLibrary                 ( getEpisodesRequest
                                                , getSeriesRequest
                                                )
import           Data                           ( GetEpisodesResponse(..)
                                                , GetSeriesResponse(..)
                                                , EpisodeResponse(..)
                                                )
import           App                            ( DefaultBridgeApp )
import           Util                           ( Pointed(..) )

class TVDBBridge bridge where
  getEpisodes :: Int -> DefaultBridgeApp bridge [EpisodeResponse]
  getSeriesName :: Int -> DefaultBridgeApp bridge String

