{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module State
  ( Handle
  , initialize
  , saveState
  , loadState
  , useChannel
  , useUsers
  ) where


import           Control.Exception (catch, SomeException)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader as Rdr
import           Data.Time (NominalDiffTime)
import qualified Domain.Channel as Ch
import qualified Domain.Users as Us
import           Text.Read (readMaybe)
----------------------------------------------------------------------
-- global state

data Handle = Handle
  { registeredUsers  :: Us.Handle
  , broadcastChannel :: Ch.Handle
  } deriving (Read, Show)


initialize :: MonadIO m => Int -> NominalDiffTime -> m Handle
initialize cacheSize cacheDuration = liftIO $ do
  regUsers <- Us.initialize
  chan <- Ch.initialize cacheSize cacheDuration
  return $ Handle regUsers chan


saveState :: Handle -> FilePath -> IO ()
saveState h fp = writeFile fp (show h)


loadState :: FilePath -> IO (Maybe Handle)
loadState fp =
  (readMaybe <$> readFile fp) `catch` (\(_ :: SomeException) -> return Nothing)


useChannel :: Rdr.MonadReader Handle m => (Ch.Handle -> m a) -> m a
useChannel f = do
  handle <- Rdr.ask
  f (broadcastChannel handle)


useUsers :: Rdr.MonadReader Handle m => (Us.Handle -> m a) -> m a
useUsers f = do
  handle <- Rdr.ask
  f (registeredUsers handle)
