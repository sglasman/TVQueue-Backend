module ControllerCommons where

import           Data                           ( UserSeasonType(..)
                                                , MyDay(..)
                                                )
import           Db                             ( UserSeason(..) )
import           Data.Time                      ( addDays )

getUserDate :: UserSeasonType -> Maybe Int -> Maybe MyDay -> Maybe MyDay
getUserDate userSeasonType numberInSeason firstAired = case userSeasonType of
  OriginalAirdates            -> firstAired
  Custom (MyDay day) interval -> do
    numberInSeason <- numberInSeason
    Just . MyDay $ addDays (toInteger $ interval * (numberInSeason - 1)) day
