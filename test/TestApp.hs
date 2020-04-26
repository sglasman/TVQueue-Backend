module TestApp where

import App
import Servant.Server (ServerError)
import Servant.Auth.Server (defaultJWTSettings, generateKey)
import DbBackend (SqliteBackend)
import TestDb (testBackend)
import OutApp
import TestBridge
import Err
import Data.Map (empty)
import Control.Monad.IO.Class (liftIO)

evalInAppTest
  :: DefaultInApp a -> IO (Either ServerError (a, DefaultInAppState))
evalInAppTest app = do
  jwtSettings <- defaultJWTSettings <$> generateKey
  evalAppFromState (InAppState jwtSettings testBackend) app
  
evalOutAppTest :: TestOutApp a -> IO (Either OutErr (a, BridgeAppState TestBridge SqliteBackend))
evalOutAppTest = evalAppFromState (BridgeAppState (TestBridge empty) testBackend)

type TestOutApp a = DefaultBridgeApp TestBridge a

inToTest :: DefaultInApp a -> TestOutApp a
inToTest inApp = do
  jwtSettings <- liftIO (defaultJWTSettings <$> generateKey)
  fmapOtherArgs (InAppState jwtSettings testBackend) (const emptyErr) inApp