{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Channel
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
import           Data.Time (getCurrentTime)
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
    time <- getCurrentTime
    case msg of
      Broadcast senderName text ->
        WS.sendTextData connection (encode $ Msgs.Message senderName text time False $ MD.renderHtml text)
      Whisper receiverId senderName text
        | receiverId == uId ->
          WS.sendTextData connection (encode $ Msgs.Message senderName text time True $ MD.renderHtml text)
        | otherwise -> pure ()

