{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module State
  ( Handle
  , registeredUsers
  , initialize
  , useChannel
  , useUsers
  ) where


import qualified Domain.Channel as Ch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader as Rdr
import qualified Domain.Users as Us

----------------------------------------------------------------------
-- global state

data Handle = Handle
  { registeredUsers  :: Us.Handle
  , broadcastChannel :: Ch.Handle
  }


initialize :: MonadIO m => Int -> m Handle
initialize cacheSize = liftIO $ do
  regUsers <- Us.initialize
  chan <- Ch.initialize cacheSize
  return $ Handle regUsers chan


useChannel :: Rdr.MonadReader Handle m => (Ch.Handle -> m a) -> m a
useChannel f = do
  handle <- Rdr.ask
  f (broadcastChannel handle)


useUsers :: Rdr.MonadReader Handle m => (Us.Handle -> m a) -> m a
useUsers f = do
  handle <- Rdr.ask
  f (registeredUsers handle)
