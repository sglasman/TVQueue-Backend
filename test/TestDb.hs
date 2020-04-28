{-# LANGUAGE OverloadedStrings #-}

module TestDb where

import           DbBackend                      ( SqliteBackend(..) )

testBackend :: SqliteBackend
testBackend = SqliteBackend "test.db"
