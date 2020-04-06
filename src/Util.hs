module Util where

import qualified Data.Map                      as M

mapToMap :: (Ord k) => (k -> v) -> [k] -> M.Map k v
mapToMap f = M.fromList . map (\k -> (k, f k))
