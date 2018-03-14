{-# LANGUAGE FlexibleContexts #-}
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
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (encode)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Maybe (mapMaybe)
import           Data.Time (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import qualified Model.Markdown as MD
import qualified Model.Messages as Msgs
import qualified Model.User as U
import qualified Network.WebSockets.Connection as WS
import           Servant (ServantErr, errBody)
import           Servant.Server (err406)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Read (Read(..))

----------------------------------------------------------------------
-- global state

data Handle = Handle
  { broadcastChannel :: STM.TChan ChanMessage
  , messageCacheSize :: Int
  , cacheDuration    :: NominalDiffTime
  , recentMessages   :: STM.TVar (Msgs.MessageId, [(Msgs.MessageId, Maybe U.UserId, Msgs.Message)])
  , lastPosted       :: STM.TVar (Map.Map U.UserId UTCTime)
  }


instance Read Handle where
  readPrec = intoHandle <$> readPrec
    where
      intoHandle :: (Int, Int, (Msgs.MessageId, [(Msgs.MessageId, Maybe U.UserId, Msgs.Message)]), Map.Map U.UserId UTCTime) -> Handle
      intoHandle (cSz, dur, msgs, posted) = Handle
        (unsafePerformIO $ STM.newTChanIO)
        cSz
        (fromIntegral dur)
        (unsafePerformIO $ STM.newTVarIO msgs)
        (unsafePerformIO $ STM.newTVarIO posted)

instance Show Handle where
  show (Handle _ cSz dur msgs posted) =
    show (cSz, (ceiling dur :: Int), unsafePerformIO $ STM.readTVarIO msgs, unsafePerformIO $ STM.readTVarIO posted)


data ChanMessage =
  ChanMessage (Maybe U.UserId) ByteString


initialize :: Int -> NominalDiffTime -> IO Handle
initialize cacheSize duration = do
  chan      <- STM.newBroadcastTChanIO
  msgMap    <- STM.newTVarIO (0, [])
  postedMap <- STM.newTVarIO Map.empty
  return $ Handle chan cacheSize duration msgMap postedMap


newMessage :: MonadIO m => Handle -> Maybe U.UserId -> (Msgs.MessageId -> Msgs.Message) -> m Msgs.Message
newMessage handle receiverId createMsg = do
  cleanCache handle
  liftIO $ atomically $ do
    (nextId, msgs) <- STM.readTVar (recentMessages handle)
    let msg = createMsg nextId
        msgs' = take (messageCacheSize handle) $ (nextId, receiverId, msg) : msgs
    STM.writeTVar (recentMessages handle) (nextId + 1, msgs')
    return msg


getCachedMessages :: MonadIO m => Maybe U.UserId -> Handle -> m [(Msgs.MessageId, Msgs.Message)]
getCachedMessages uid handle = do
  cleanCache handle
  liftIO $ atomically $
    mapMaybe accessible . snd
    <$> STM.readTVar (recentMessages handle)
  where
    accessible (mid, Nothing, msg) =
      Just (mid, msg)
    accessible (mid, recid, msg) =
      if uid == recid
      then Just (mid,msg)
      else Nothing


cleanCache :: MonadIO m => Handle -> m ()
cleanCache handle = liftIO $ do
  now <- getCurrentTime
  let stillValid (_,_,msg) =
        now `diffUTCTime` Msgs._msgTime msg <= cacheDuration handle
  atomically $ STM.modifyTVar (recentMessages handle) (\(nextId, msgs) -> (nextId, filter stillValid msgs))


broadcast :: (MonadError ServantErr m, MonadIO m) => Handle -> U.User -> MD.Markdown -> m ()
broadcast handle sender text = do
  time <- liftIO $ getCurrentTime
  assertNoPostSpam handle (U._userId sender) time
  msg <- newMessage handle Nothing $ Msgs.createPost time (U._userName sender) text False
  liftIO $ putStrLn $ "sending message to all users: " ++ show msg
  liftIO $ atomically $ STM.writeTChan (broadcastChannel handle) (ChanMessage Nothing $ encode msg)


whisper :: (MonadError ServantErr m, MonadIO m) => Handle -> U.UserId -> U.User -> MD.Markdown -> m ()
whisper handle receiverId sender text = do
  time <- liftIO $ getCurrentTime
  assertNoPostSpam handle (U._userId sender) time
  msg <- newMessage handle (Just receiverId) $ Msgs.createPost time (U._userName sender) text True
  liftIO $ putStrLn $ "sending message to " ++ show receiverId ++ ": " ++ show msg
  liftIO $ atomically $ STM.writeTChan (broadcastChannel handle) (ChanMessage (Just receiverId) $ encode msg)


systemMessage :: MonadIO m => Handle -> MD.Markdown -> m ()
systemMessage handle text = liftIO $ do
  time <- getCurrentTime
  msg <- newMessage handle Nothing $ Msgs.createSystem time text
  atomically $ STM.writeTChan (broadcastChannel handle) (ChanMessage Nothing $ encode msg)


assertNoPostSpam :: (MonadError ServantErr m, MonadIO m) => Handle -> U.UserId -> UTCTime -> m ()
assertNoPostSpam handle userId time = do
  secsSinceLastPost <- fromMaybe 100 . fmap (time `diffUTCTime`) . Map.lookup userId <$> liftIO (STM.readTVarIO $ lastPosted handle)
  if secsSinceLastPost <= 1
    then throwError (err406 { errBody =  "users are only allowed to post at least one second apart"})
    else liftIO $ atomically $
      STM.modifyTVar (lastPosted handle) (Map.insert userId time)


connectUser :: forall m . (MonadCatch m, MonadIO m) => Handle -> Maybe (U.User) -> WS.Connection -> m ()
connectUser (Handle broadcastChan _ _ _ _) user connection = do
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
