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
   :<|> Capture "userid" U.UserId :> "stream" :> WebSocket
  )

----------------------------------------------------------------------
-- Servant-Handler


handler :: S.Handle -> ServerT API Handler
handler handle = enter (toServantHandler handle) $
  messageReceivedHandler
  :<|> whisperReceiveHandler
  :<|> getMessagesHandler
  :<|> messageWebsocketHandler


messageReceivedHandler :: Msgs.SendMessage -> ChatHandler NoContent
messageReceivedHandler sendMsg = do
  foundUser <- getUser (sendMsg^.Msgs.sendSender)
  case foundUser of
    Just user -> do
      broadcastMessage (user^.U.userName) (sendMsg^.Msgs.sendText)
      return NoContent
    Nothing ->
      throwError $ err404 { errBody = "user not found" }


getMessagesHandler :: U.UserId -> Maybe Msgs.MessageId -> ChatHandler [Msgs.Message]
getMessagesHandler userId fromNo =
  let msgFilter = map snd . maybe id (\no -> filter (\(n,_) -> n >= no)) fromNo
  in msgFilter <$> S.useChannel (Ch.getCachedMessages userId)


whisperReceiveHandler :: Msgs.WhisperMessage -> ChatHandler NoContent
whisperReceiveHandler sendMsg = do
  foundSender <- getUser (sendMsg^.Msgs.whispSender)
  foundReceiverId <- getUserId (sendMsg^.Msgs.whispReceiver)
  case (foundSender, foundReceiverId) of
    (Just sender, Just receiverId) -> do
      whisperMessage receiverId (sender^.U.userName) (sendMsg^.Msgs.whispText)
      return NoContent
    _ ->
      throwError $ err404 { errBody = "unknown sender or receiver" }


messageWebsocketHandler :: U.UserId -> Connection -> ChatHandler ()
messageWebsocketHandler uId connection = do
  userOpt <- S.useUsers (\uh -> Users.getUser uh uId)
  case userOpt of
    Just user ->
      S.useChannel (\ch -> Ch.connectUser ch user connection)
    Nothing -> do
      throwError $ err404 { errBody = "unknown user" }
