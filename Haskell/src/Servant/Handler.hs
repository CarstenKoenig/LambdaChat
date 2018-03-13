{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Servant.Handler
  ( ChatHandler
  , toServantHandler
  , listAllUsers
  , getUser
  , getUserId
  , loginUser
  , logoutUser
  , broadcastMessage
  , whisperMessage
  , systemMessage
  ) where

import           Control.Monad.Except (throwError)
import qualified Control.Monad.Reader as R
import           Data.Text (Text)
import qualified Domain.Channel as Ch
import qualified Domain.Users as Us
import qualified Model.Markdown as MD
import qualified Model.User as U
import           Servant
import           Servant.Server (err401)
import qualified State as S


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
    Just newId -> do
      return newId
    Nothing ->
        throwError $ err401 { errBody = "there is already a user with this name logged in and you don't know his password" }


logoutUser :: U.UserId -> ChatHandler NoContent
logoutUser uid = do
  S.useUsers (\uh -> Us.logoutUser uh uid)
  return NoContent


broadcastMessage :: U.User -> MD.Markdown -> ChatHandler ()
broadcastMessage sender text =
  S.useChannel (\ch -> Ch.broadcast ch sender text)


whisperMessage :: U.UserId -> U.User -> MD.Markdown -> ChatHandler ()
whisperMessage receiverId sender text =
  S.useChannel (\ch -> Ch.whisper ch receiverId sender text)


systemMessage :: MD.Markdown -> ChatHandler ()
systemMessage text =
  S.useChannel (\ch -> Ch.systemMessage ch text)
  
