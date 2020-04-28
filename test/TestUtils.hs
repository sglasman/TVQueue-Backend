module TestUtils where

import           Control.Exception              ( SomeException )
import           DbBackend                      ( ProvidesDbBackend )
import           Database.Persist               ( Entity(..)
                                                , Key
                                                )
import           Db                             ( DbAction
                                                , runDbAction
                                                )
import           Util                           ( orFail )
import           App                            ( App )
import           Err                            ( OutErr
                                                , emptyErr
                                                )
import           Data.Time.Calendar             ( Day
                                                , addDays
                                                , fromGregorian
                                                )
import           Data                           ( EpisodeResponse(..)
                                                , MyDay(..)
                                                )

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

dbGetOrThrow
  :: (ProvidesDbBackend state)
  => err
  -> DbAction (Maybe (Entity a))
  -> App err state (Entity a)
dbGetOrThrow err = orFail err . runDbAction

dbGetOrThrowEmpty
  :: (ProvidesDbBackend state)
  => DbAction (Maybe (Entity a))
  -> App OutErr state (Entity a)
dbGetOrThrowEmpty = dbGetOrThrow emptyErr

sampleSeason1 :: Day -> Int -> [EpisodeResponse]
sampleSeason1 startDate numberOfEps = map
  (\n -> EpisodeResponse
    (Just n)
    (Just 1)
    (Just . MyDay . (addDays . toInteger $ 7 * n) $ startDate)
    Nothing
    n
  )
  [1 .. numberOfEps]

sampleSeason2 :: [EpisodeResponse]
sampleSeason2 = map
  (\n -> EpisodeResponse (Just n)
                         (Just 2)
                         (Just . MyDay $ fromGregorian 2021 1 1)
                         Nothing
                         (n + 100)
  )
  [1, 2, 3, 4]
