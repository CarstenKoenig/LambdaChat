{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler
  ( ChatHandler
  , toServantHandler
  , listAllUsers
  , getUser
  , getUserId
  , loginUser
  , broadcastMessage
  , whisperMessage
  ) where

import qualified Channel as Ch
import Control.Monad.Except (throwError)
import qualified Control.Monad.Reader as R
import Data.Text (Text)
import qualified Model.Markdown as MD
import qualified Model.User as U
import Servant
import Servant.Server (err401)
import qualified State as S
import qualified Users as Us


type ChatHandler = R.ReaderT S.Handle Handler

toServantHandler :: S.Handle -> ChatHandler :~> Handler
toServantHandler handle = NT (`R.runReaderT` handle)


listAllUsers :: ChatHandler [U.PublicInfo]
listAllUsers =
  S.useUsers Us.listAll


getUser :: U.UserId -> ChatHandler (Maybe U.User)
getUser uId =
  S.useUsers (`Us.getUser` uId)


getUserId :: U.UserName -> ChatHandler (Maybe U.UserId)
getUserId user =
  S.useUsers (`Us.getUserId` user)


loginUser :: U.UserName -> Text -> ChatHandler U.UserId
loginUser name password = do
  res <- S.useUsers (\uh -> Us.loginUser uh name password)
  case res of
    Just newId ->
      return newId
    Nothing ->
        throwError $ err401 { errBody = "there is already a user with this name logged in and you don't know his password" }


broadcastMessage :: U.UserName -> MD.Markdown -> ChatHandler ()
broadcastMessage senderName text =
  S.useChannel (\ch -> Ch.broadcast ch senderName text)


whisperMessage :: U.UserId -> U.UserName -> MD.Markdown -> ChatHandler ()
whisperMessage receiverId senderName text =
  S.useChannel (\ch -> Ch.whisper ch receiverId senderName text)
