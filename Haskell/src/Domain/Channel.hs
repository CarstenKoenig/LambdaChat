{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Domain.Channel
  ( Handle
  , initialize
  , broadcast
  , whisper
  , connectUser
  ) where

import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TChan as STM
import           Control.Monad (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (encode)
import qualified Model.Markdown as MD
import qualified Model.Messages as Msgs
import qualified Model.User as U
import qualified Network.WebSockets.Connection as WS
----------------------------------------------------------------------
-- global state

newtype Handle = Handle (STM.TChan ChanMessage)


data ChanMessage
  = Broadcast U.UserName MD.Markdown
  | Whisper U.UserId U.UserName MD.Markdown


initialize :: IO Handle
initialize = do
  chan <- STM.newBroadcastTChanIO
  return $ Handle chan


broadcast :: MonadIO m => Handle -> U.UserName -> MD.Markdown -> m ()
broadcast (Handle broadcastChan) senderName text = liftIO $ atomically $
  STM.writeTChan broadcastChan (Broadcast senderName text)


whisper :: MonadIO m => Handle -> U.UserId -> U.UserName -> MD.Markdown -> m ()
whisper (Handle broadcastChan) receiverId senderName text = liftIO $ atomically $
  STM.writeTChan broadcastChan (Whisper receiverId senderName text)


connectUser :: MonadIO m => Handle -> U.UserId -> WS.Connection -> m ()
connectUser (Handle broadcastChan) uId connection = liftIO $ do
  chan <- atomically $ STM.dupTChan broadcastChan

  WS.forkPingThread connection 10

  forever $ do
    msg <- atomically $ STM.readTChan chan
    case msg of
      Broadcast senderName text -> do
        post <- Msgs.createPost senderName text False
        WS.sendTextData connection (encode post)
      Whisper receiverId senderName text
        | receiverId == uId -> do
            post <- Msgs.createPost senderName text True
            WS.sendTextData connection (encode post)
        | otherwise -> pure ()

