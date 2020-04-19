module ControllerCommons where

import           Data                           ( MyDay(..)
                                                , UserSeasonType(..)
                                                )
import           Db                             ( UserSeason(..) )
import           Data.Time                      ( addDays )

getUserDate :: UserSeason -> Maybe MyDay -> Maybe MyDay
getUserDate userSeason firstAired = case userSeasonUserSeasonType userSeason of
  OriginalAirdates -> firstAired
  Custom (MyDay day) interval ->
    Just . MyDay $ addDays (toInteger interval) day
