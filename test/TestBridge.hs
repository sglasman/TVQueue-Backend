module TestBridge where

import qualified Data.Map                      as M
import           Data                           ( EpisodeResponse )
import           Control.Monad.State            ( gets
                                                , modify
                                                )
import           Control.Monad.Except           ( throwError )
import           TVDBBridge                     ( TVDBBridge(..) )
import           App                            ( BridgeAppState(..)
                                                , DefaultBridgeApp(..)
                                                )
import           Err
import           Data.Map                       ( insert )

newtype TestBridge = TestBridge {
  series :: M.Map Int (String, [EpisodeResponse])
}

instance TVDBBridge TestBridge where
  getEpisodes seriesId = do
    seriesMap <- gets $ series . bridge
    maybe (throwError $ OutErr Nothing "") (return . snd)
      $ M.lookup seriesId seriesMap
  getSeriesName seriesId = do
    seriesMap <- gets $ series . bridge
    maybe (throwError $ OutErr Nothing "") (return . fst)
      $ M.lookup seriesId seriesMap

putTestBridge
  :: Int -> (String, [EpisodeResponse]) -> DefaultBridgeApp TestBridge ()
putTestBridge seriesId pair = modify
  (\s -> s
    { bridge = (bridge s) { series = insert seriesId pair (series $ bridge s) }
    }
  )
