{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.MessageApi
  ( API
  , handler
  ) where

import           Control.Lens ((^.))
import           Control.Monad.Except (throwError)
import qualified Domain.Channel as Ch
import qualified Domain.Users as Users
import qualified Model.Messages as Msgs
import qualified Model.User as U
import           Network.WebSockets.Connection (Connection)
import           Servant
import           Servant.API.WebSocket
import           Servant.Handler
import           Servant.Server (err404)
import qualified State as S

type API =
  "messages" :>
  (ReqBody '[JSON] Msgs.SendMessage :> PostNoContent '[JSON] NoContent
   :<|> "whisper" :> ReqBody '[JSON] Msgs.WhisperMessage :> PostNoContent '[JSON] NoContent
   :<|> Capture "userid" U.UserId :> QueryParam "fromid" Msgs.MessageId :> Get '[JSON] [Msgs.Message]
   :<|> "public" :> QueryParam "fromid" Msgs.MessageId :> Get '[JSON] [Msgs.Message]
   :<|> "stream" :> Capture "userid" U.UserId :> WebSocket
   :<|> "stream" :> "public" :> WebSocket
  )

----------------------------------------------------------------------
-- Servant-Handler


handler :: S.Handle -> ServerT API Handler
handler handle = enter (toServantHandler handle) $
  messageReceivedHandler
  :<|> whisperReceiveHandler
  :<|> getUserMessagesHandler
  :<|> getPublicMessagesHandler
  :<|> userStreamWebsocketHandler
  :<|> publicStreamWebsocketHandler


messageReceivedHandler :: Msgs.SendMessage -> ChatHandler NoContent
messageReceivedHandler sendMsg = do
  foundUser <- getUser (sendMsg^.Msgs.sendSender)
  case foundUser of
    Just user -> do
      broadcastMessage user (sendMsg^.Msgs.sendText)
      return NoContent
    Nothing ->
      throwError $ err404 { errBody = "user not found" }


getUserMessagesHandler :: U.UserId -> Maybe Msgs.MessageId -> ChatHandler [Msgs.Message]
getUserMessagesHandler userId fromNo =
  let msgFilter = map snd . maybe id (\no -> filter (\(n,_) -> n >= no)) fromNo
  in msgFilter <$> S.useChannel (Ch.getCachedMessages $ Just userId)


getPublicMessagesHandler :: Maybe Msgs.MessageId -> ChatHandler [Msgs.Message]
getPublicMessagesHandler fromNo =
  let msgFilter = map snd . maybe id (\no -> filter (\(n,_) -> n >= no)) fromNo
  in msgFilter <$> S.useChannel (Ch.getCachedMessages Nothing)


whisperReceiveHandler :: Msgs.WhisperMessage -> ChatHandler NoContent
whisperReceiveHandler sendMsg = do
  foundSender <- getUser (sendMsg^.Msgs.whispSender)
  foundReceiverId <- getUserId (sendMsg^.Msgs.whispReceiver)
  case (foundSender, foundReceiverId) of
    (Just sender, Just receiverId) -> do
      whisperMessage receiverId sender (sendMsg^.Msgs.whispText)
      return NoContent
    _ ->
      throwError $ err404 { errBody = "unknown sender or receiver" }


userStreamWebsocketHandler :: U.UserId -> Connection -> ChatHandler ()
userStreamWebsocketHandler uId connection = do
  user <- S.useUsers (\uh -> Users.getUser uh uId)
  case user of
    Nothing -> authErr
    Just _  -> S.useChannel (\ch -> Ch.connectUser ch user connection)
  where
    authErr = throwError $ err404 { errBody = "unknown user" }


publicStreamWebsocketHandler :: Connection -> ChatHandler ()
publicStreamWebsocketHandler connection = do
  S.useChannel (\ch -> Ch.connectUser ch Nothing connection)
