{-# LANGUAGE OverloadedStrings #-}

module InboundControllerQCTest where

import           Test.QuickCheck                ( quickCheck )
import           Test.QuickCheck.Monadic        ( monadicIO
                                                , assert
                                                , PropertyM
                                                )
import qualified Test.QuickCheck.Monadic       as Q
                                                ( run )
import           App
import           InboundController
import           RequestTypes
import           TestInstances
import           Data.Either                    ( isRight
                                                , isLeft
                                                )
import           Db
import           System.Directory               ( removeFile )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Exception              ( catch
                                                , SomeException
                                                )
import           Control.Monad                  ( void )
import           Data.Text                      ( pack
                                                , unpack
                                                )

run = Q.run . evalInAppTest



setup =
  run $ liftIO (catch (removeFile "test.db") ignoreException) >> doMigrateAll

createUserTest = quickCheck
  (\cur -> monadicIO $ do
    setup
    result1 <- run $ handleCreateUserRequest cur
    assert (isRight result1)
    result2 <- run $ handleCreateUserRequest cur
    assert (isLeft result2)
  )

loginTest = quickCheck
  (\email pass -> monadicIO $ do
    setup
    result1 <- run . handleCreateUserRequest $ CreateUserRequest email pass
    assert (isRight result1)
    result2 <- run . handleLoginRequest $ LoginRequest email pass
    assert (isRight result2)
    result3 <- run . handleLoginRequest $ LoginRequest
      email
      (pack $ unpack pass ++ "bad")
    assert (isLeft result3)
  )
