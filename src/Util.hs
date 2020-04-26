{-# LANGUAGE MultiParamTypeClasses #-}
module Util where

import qualified Data.Map                      as M
import Control.Monad.Except (MonadError, throwError)
import Data.Maybe (maybe)

mapToMap :: (Ord k) => (k -> v) -> [k] -> M.Map k v
mapToMap f = M.fromList . map (\k -> (k, f k))

class Pointed a where
  point :: a

orFail :: (MonadError err m) => err -> m (Maybe a) -> m a
orFail e ma = do
  a <- ma
  maybe (throwError e) return a