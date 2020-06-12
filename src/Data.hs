{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Data
  ( Creds(..)
  , MyDay(..)
  , SeasonType(..)
  , UserSeasonType(..)
  , SeriesType(..)
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Aeson
import           Data.Text
import           Data.Time                      ( Day
                                                , defaultTimeLocale
                                                , parseTimeM
                                                )
import           Database.Persist.Sql           ( PersistField
                                                , PersistFieldSql
                                                , PersistValue(..)
                                                , toPersistValue
                                                )
import           Database.Persist.TH            ( derivePersistField )
import           GHC.Generics                   ( Generic )

data Creds = Creds {
  username :: String,
  userkey  :: String,
  apikey   :: String
} deriving (Generic, Show, ToJSON)

newtype MyDay = MyDay { getDay :: Day } deriving (Show, Read, Eq, Generic, Ord)
derivePersistField "MyDay"

instance FromJSON MyDay where
  parseJSON v = withText
    ""
    (maybe (fail $ "Could not parse date " ++ show v) (pure . MyDay) . textToDay
    )
    v

timeFormatString :: String
timeFormatString = "%Y-%-m-%-d"

textToDay :: Text -> Maybe Day
textToDay = parseTimeM True defaultTimeLocale timeFormatString . unpack

instance ToJSON MyDay where
  toJSON = toJSON . getDay

data SeasonType = Ongoing | Finished | PastDump { date :: MyDay } | FutureDump { date :: MyDay } deriving (Show, Read, Eq)
derivePersistField "SeasonType"

data UserSeasonType = OriginalAirdates | Custom { startDate :: MyDay, interval :: Int } deriving (Show, Read, Eq)
derivePersistField "UserSeasonType"

instance FromJSON UserSeasonType where
  parseJSON value = parseOriginalAirdates value <|> parseCustom value   where
    parseOriginalAirdates = withText
      ""
      (\text -> if text == "originalAirdates"
        then return OriginalAirdates
        else fail ""
      )
    parseCustom = withObject
      ""
      (\o -> Custom <$> (o .: "startDate") <*> (o .: "interval"))

data SeriesType = Continuing | Ended deriving (Show, Eq, Generic, ToJSON)

instance FromJSON SeriesType where
  parseJSON = withText
    ""
    (\text -> case toLower text of
      "continuing" -> return Continuing
      "ended"      -> return Ended
      _            -> fail ""
    )



