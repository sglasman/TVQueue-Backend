module OutboundController where

import Data
import App
import TVDBAuth
import RequestLibrary

getEpisodesCollated :: Int -> DefaultApp IO [EpisodeResponse]
getEpisodesCollated id = do
  firstPage <- runAuthenticated $ getEpisodesRequest id 1
  otherPages <- mapM (runAuthenticated . getEpisodesRequest id) [2..(pageCount firstPage)]
  return $ episodes firstPage ++ (otherPages >>= episodes)

addSeries :: Int -> App IO ()
addSeries seriesId = do
  name <- 