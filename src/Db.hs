{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Db where

import Database.Persist.TH
--import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Data.Text (Text)
import Control.Monad.Trans.Reader
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Class (insert)

--connString :: ConnectionString
--connString = "host=127.0.0.1 port=5432 user=postgres password=postgres dbname=tvqbh"

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Series sql=series
      name Text
      tvdbId Int
      UniqueTvdbId tvdbId
      deriving Show Read
  |]

-- type DbAction a b =  

--addSeries :: Series -> IO SeriesId
--addSeries series = runStdoutLoggingT $ withPostgresqlConn connString $ runReaderT $ insert series