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
import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (ToJSON, encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Time (getCurrentTime)
import qualified Model.Markdown as MD
import qualified Model.Messages as Msgs
import qualified Model.User as U
import qualified Network.WebSockets.Connection as WS

----------------------------------------------------------------------
-- global state

data Handle = Handle
  { broadcastChannel :: STM.TChan ChanMessage
  , messageCacheSize :: Int
  , recentMessages   :: STM.TVar (Msgs.MessageId, [ByteString])
  }


data ChanMessage =
  ChanMessage (Maybe U.UserId) ByteString


initialize :: Int -> IO Handle
initialize cacheSize = do
  chan   <- STM.newBroadcastTChanIO
  msgMap <- STM.newTVarIO (0, [])
  return $ Handle chan cacheSize msgMap


newMessage :: (ToJSON a, MonadIO m) => Handle -> (Msgs.MessageId -> a) -> m (a, ByteString)
newMessage handle createMsg = liftIO $ atomically $ do
  (nextId, msgs) <- STM.readTVar (recentMessages handle)
  let msg = createMsg nextId
      textData = encode msg
      msgs' = drop (length msgs + 1 - messageCacheSize handle) $ msgs ++ [textData]
  STM.writeTVar (recentMessages handle) (nextId + 1, msgs')
  return (msg, textData)


broadcast :: MonadIO m => Handle -> U.UserName -> MD.Markdown -> m ()
broadcast handle senderName text = liftIO $ do
  time <- getCurrentTime
  (_, textData) <- newMessage handle $ Msgs.createPost time senderName text False
  atomically $ STM.writeTChan (broadcastChannel handle) (ChanMessage Nothing textData)


whisper :: MonadIO m => Handle -> U.UserId -> U.UserName -> MD.Markdown -> m ()
whisper handle receiverId senderName text = liftIO $ do
  time <- getCurrentTime
  (_, textData) <- newMessage handle $ Msgs.createPost time senderName text True
  atomically $ STM.writeTChan (broadcastChannel handle) (ChanMessage (Just receiverId) textData)


connectUser :: MonadIO m => Handle -> U.UserId -> WS.Connection -> m ()
connectUser (Handle broadcastChan _ _) uId connection = liftIO $ do
  chan <- atomically $ STM.dupTChan broadcastChan

  WS.forkPingThread connection 10

  forever $ do
    ChanMessage receiverId' textData <- atomically $ STM.readTChan chan
    case receiverId' of
      Just receiverId
        | receiverId == uId ->
          WS.sendTextData connection textData
      _ ->
        WS.sendTextData connection textData

