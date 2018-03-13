{-# LANGUAGE FlexibleContexts #-}
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
  , logoutUser
  ) where

import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TVar as STM
import           Control.Lens (view, set, at, _Just, (^.), (.~), makeLenses)
import           Control.Monad (unless)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Model.User as U
import           Servant (ServantErr, errBody)
import           Servant.Server (err406)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Read (Read(..))

----------------------------------------------------------------------
-- global state

data Users = Users
  { _userFromId     :: Map.Map U.UserId U.User
  , _userIdFromName :: Map.Map U.UserName U.UserId
  } deriving (Read, Show)

makeLenses ''Users

newtype Handle = Handle (STM.TVar Users)

instance Read Handle where
  readPrec = intoHandle <$> readPrec
    where intoHandle usrs = Handle $ unsafePerformIO (STM.newTVarIO usrs)

instance Show Handle where
  show (Handle uvar) = show (unsafePerformIO $ STM.readTVarIO uvar)


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


loginUser :: (MonadError ServantErr m, MonadIO m) => Handle -> U.UserName -> Text -> m (Maybe U.UserId)
loginUser handle name password = do
  unless (T.length name >= 4) $ throwError $ err406 { errBody =  "your name should have at least 4 characters - sorry"}
  alreadyRegisteredId <- readRegisteredUsers handle (view (userIdFromName . at name))
  alreadyRegistered <- maybe (return Nothing) (getUser handle) alreadyRegisteredId
  case alreadyRegistered of
    Nothing -> do
      newId <- liftIO U.newUserId
      let user = U.User newId name password True
      modifyRegisteredUsers handle (set (userFromId . at newId) (Just user) . set (userIdFromName . at name) (Just newId))
      return $ Just newId
    Just found
      | found^.U.userPassword == password -> do
          modifyRegisteredUsers handle (userFromId . at (found^.U.userId) . _Just . U.userIsOnline .~ True)
          return $ Just $ found^.U.userId
      | otherwise ->
          return Nothing


logoutUser :: MonadIO m => Handle -> U.UserId -> m ()
logoutUser handle uId = do
  modifyRegisteredUsers handle (userFromId . at uId . _Just . U.userIsOnline .~ False)


readRegisteredUsers :: MonadIO m => Handle -> (Users -> a) -> m a
readRegisteredUsers (Handle varUsers) f =
  liftIO (atomically $ f <$> STM.readTVar varUsers)


modifyRegisteredUsers :: MonadIO m => Handle -> (Users -> Users) -> m ()
modifyRegisteredUsers (Handle varUsers) modify =
  liftIO $ atomically $ STM.modifyTVar varUsers modify
