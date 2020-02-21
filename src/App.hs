{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module App where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Err
import Control.Monad.Error.Class

newtype App m a = App { runApp :: StateT String (ExceptT Err m) a}

deriving instance Functor m => Functor (App m)
deriving instance Monad m => Applicative (App m)
deriving instance Monad m => Monad (App m)
deriving instance MonadIO m => MonadIO (App m)
deriving instance Monad m => MonadState String (App m)
deriving instance Monad m => MonadError Err (App m)

showApp :: (Show a) => App IO a -> IO ()
showApp (App stateT) = runExceptT (runStateT stateT "") >>= (print . show)