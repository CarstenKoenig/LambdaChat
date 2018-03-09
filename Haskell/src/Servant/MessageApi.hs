{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.MessageApi
  ( API
  , handler
  ) where

import qualified Channel as Ch
import Control.Lens ((^.)) 
import Control.Monad.Except (throwError)
import qualified Control.Monad.Reader as R
import Handler
import qualified Model.Messages as Msgs
import qualified Model.User as U
import Network.WebSockets.Connection (Connection)
import Servant
import Servant.API.WebSocket
import Servant.Server (err404)
import qualified State as S


type API =
  "messages" :>
  (ReqBody '[JSON] Msgs.SendMessage :> PostNoContent '[JSON] NoContent
   :<|> "whisper" :> ReqBody '[JSON] Msgs.WhisperMessage :> PostNoContent '[JSON] NoContent
   :<|> Capture "userid" U.UserId :> WebSocket
  )

----------------------------------------------------------------------
-- Servant-Handler


handler :: ServerT API ChatHandler
handler = messageReceivedHandler :<|> whisperReceiveHandler :<|> messageWebsocketHandler


messageReceivedHandler :: Msgs.SendMessage -> ChatHandler NoContent
messageReceivedHandler sendMsg = do
  foundUser <- getUser (sendMsg^.Msgs.sendSender)
  case foundUser of
    Just user -> do
      broadcastMessage (user^.U.userName) (sendMsg^.Msgs.sendText)
      return NoContent
    Nothing ->
      throwError $ err404 { errBody = "user not found" }


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


messageWebsocketHandler :: U.UserId -> Connection -> R.ReaderT S.Handle Handler ()
messageWebsocketHandler uId connection =
  S.useChannel (\ch -> Ch.connectUser ch uId connection)
