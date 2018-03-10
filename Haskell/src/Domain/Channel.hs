{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Channel
  ( Handle
  , initialize
  , getCachedMessages
  , broadcast
  , whisper
  , systemMessage
  , connectUser
  ) where

import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (forever)
import           Control.Monad.Catch (MonadCatch, catch, SomeException)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe (mapMaybe, fromMaybe)
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
  , recentMessages   :: STM.TVar (Msgs.MessageId, [(Msgs.MessageId, Maybe U.UserId, Msgs.Message)])
  }


data ChanMessage =
  ChanMessage (Maybe U.UserId) ByteString


initialize :: Int -> IO Handle
initialize cacheSize = do
  chan   <- STM.newBroadcastTChanIO
  msgMap <- STM.newTVarIO (0, [])
  return $ Handle chan cacheSize msgMap


newMessage :: MonadIO m => Handle -> Maybe U.UserId -> (Msgs.MessageId -> Msgs.Message) -> m Msgs.Message
newMessage handle receiverId createMsg = liftIO $ atomically $ do
  (nextId, msgs) <- STM.readTVar (recentMessages handle)
  let msg = createMsg nextId
      msgs' = take (messageCacheSize handle) $ msgs ++ [(nextId, receiverId, msg)]
  STM.writeTVar (recentMessages handle) (nextId + 1, msgs')
  return msg


getCachedMessages :: MonadIO m => U.UserId -> Handle -> m [(Msgs.MessageId, Msgs.Message)]
getCachedMessages uid handle = liftIO $ atomically $
  reverse . mapMaybe accessible . snd
  <$> STM.readTVar (recentMessages handle)
  where
    accessible (mid, recid, msg) =
      if uid == fromMaybe uid recid
      then Just (mid,msg)
      else Nothing


broadcast :: MonadIO m => Handle -> U.UserName -> MD.Markdown -> m ()
broadcast handle senderName text = liftIO $ do
  time <- getCurrentTime
  msg <- newMessage handle Nothing $ Msgs.createPost time senderName text False
  putStrLn $ "sending message to all users: " ++ show msg
  atomically $ STM.writeTChan (broadcastChannel handle) (ChanMessage Nothing $ encode msg)


whisper :: MonadIO m => Handle -> U.UserId -> U.UserName -> MD.Markdown -> m ()
whisper handle receiverId senderName text = liftIO $ do
  time <- getCurrentTime
  msg <- newMessage handle (Just receiverId) $ Msgs.createPost time senderName text True
  putStrLn $ "sending message to " ++ show receiverId ++ ": " ++ show msg
  atomically $ STM.writeTChan (broadcastChannel handle) (ChanMessage (Just receiverId) $ encode msg)


systemMessage :: MonadIO m => Handle -> MD.Markdown -> m ()
systemMessage handle text = liftIO $ do
  time <- getCurrentTime
  msg <- newMessage handle Nothing $ Msgs.createSystem time text
  atomically $ STM.writeTChan (broadcastChannel handle) (ChanMessage Nothing $ encode msg)


connectUser :: forall m . (MonadCatch m, MonadIO m) => Handle -> Maybe (U.User) -> WS.Connection -> m ()
connectUser (Handle broadcastChan _ _) user connection = do
  liftIO $ putStrLn $ "user " ++ show (maybe "<annonymous>" U._userName user) ++ " connected"
  go `catch` closed
  where
    go = liftIO $ do
      chan <- atomically $ STM.dupTChan broadcastChan
      WS.forkPingThread connection 10

      forever $ do
        ChanMessage receiverId' textData <- atomically $ STM.readTChan chan
        case receiverId' of
          Just receiverId
            | Just receiverId == (U._userId <$> user) ->
              WS.sendTextData connection textData
          _ ->
            WS.sendTextData connection textData

    closed :: SomeException -> m ()
    closed _ = liftIO $
      putStrLn $ "user " ++ show (maybe "<annonymous>" U._userName user) ++ " disconnected"
