{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Domain.Users
  ( Handle
  , initialize
  , listAll
  , getUser
  , getUserId
  , loginUser
  ) where

import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TVar as STM
import Control.Lens (view, set, at, (^.), makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Model.User as U

----------------------------------------------------------------------
-- global state

data Users = Users
  { _userFromId     :: Map.Map U.UserId U.User
  , _userIdFromName :: Map.Map U.UserName U.UserId
  }

makeLenses ''Users

newtype Handle = Handle (STM.TVar Users)


initialize :: MonadIO m => m Handle
initialize = liftIO $ do
  var <- STM.newTVarIO (Users Map.empty Map.empty)
  return $ Handle var


listAll :: MonadIO m => Handle -> m [U.PublicInfo]
listAll handle =
  readRegisteredUsers handle (fmap toInfo . Map.elems . _userFromId)
  where toInfo user = U.publicInfo user


getUser :: MonadIO m => Handle -> U.UserId -> m (Maybe U.User)
getUser handle uId =
  readRegisteredUsers handle getUser'
  where
    getUser' = view (userFromId . at uId)


getUserId :: MonadIO m => Handle -> U.UserName -> m (Maybe U.UserId)
getUserId handle user =
  readRegisteredUsers handle (view $ userIdFromName . at user)


loginUser :: MonadIO m => Handle -> U.UserName -> Text -> m (Maybe U.UserId)
loginUser handle name password = do
  alreadyRegisteredId <- readRegisteredUsers handle (view (userIdFromName . at name))
  alreadyRegistered <- maybe (return Nothing) (getUser handle) alreadyRegisteredId
  case alreadyRegistered of
    Nothing -> do
      newId <- liftIO U.newUserId
      let user = U.User newId name password
      modifyRegisteredUsers handle (set (userFromId . at newId) (Just user) . set (userIdFromName . at name) (Just newId))
      return $ Just newId
    Just found
      | found^.U.userPassword == password ->
          return $ Just $ found^.U.userId
      | otherwise ->
          return Nothing


readRegisteredUsers :: MonadIO m => Handle -> (Users -> a) -> m a
readRegisteredUsers (Handle varUsers) f =
  liftIO (atomically $ f <$> STM.readTVar varUsers)


modifyRegisteredUsers :: MonadIO m => Handle -> (Users -> Users) -> m ()
modifyRegisteredUsers (Handle varUsers) modify =
  liftIO $ atomically $ STM.modifyTVar varUsers modify
