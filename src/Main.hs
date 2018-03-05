{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Arrow ((***))
import Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as SV
import Control.Lens (view, over, set, at, _1, _2, makeLenses)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader as R
import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as URnd
import GHC.Generics (Generic)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant


----------------------------------------------------------------------
-- User Data

type UserId = UUID
type UserName = Text

data User = User
  { _userId   :: UserId
  , _userName :: UserName
  } deriving (Eq, Show, Generic)


instance ToJSON User


newUserId :: IO UserId
newUserId = URnd.nextRandom


data Users = Users
  { _userFromId :: Map UserId User
  , _userIdFromName :: Map UserName UserId
  }
makeLenses ''Users


----------------------------------------------------------------------
-- global state

data Environment = Environment
  { registeredUsers :: SV.TVar Users
  }

newEnv :: IO Environment
newEnv = do
  regUsers <- atomically $ SV.newTVar (Users Map.empty Map.empty)
  return $ Environment regUsers


----------------------------------------------------------------------
-- Register Data

data Registration = Registration
  { registrationName :: UserName
  } deriving (Eq, Show, Generic)


instance FromJSON Registration


----------------------------------------------------------------------
-- Messages

data Message = Message
  { _msgSender :: UserName
  , _msgText   :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Message

makeLenses ''Message


data SendMessage = SendMessage
  { _sendSender :: UserId
  , _sendText   :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON SendMessage

makeLenses ''SendMessage


----------------------------------------------------------------------
-- entry point

main :: IO ()
main = do
  env <- newEnv

  putStrLn "serving app on http://localhost:8081"
  Warp.run 8081 $ servantApp env


servantApp :: Environment -> Application
servantApp env =
  serve (Proxy :: Proxy API) $ enter (chatMToHandler env) (userHandler :<|> messagesHandler)

----------------------------------------------------------------------
-- Servant-API

type API = UserAPI :<|> MessageAPI

type UserAPI =
  "users" :>
  (Capture "userid" UserId :> Get '[JSON] User
   :<|> "register" :> ReqBody '[JSON] Registration :> Post '[JSON] UserId)


type MessageAPI =
  "messages" :> ReqBody '[JSON] SendMessage :> PostNoContent '[JSON] NoContent

----------------------------------------------------------------------
-- Servant-Handler

userHandler :: ServerT UserAPI ChatM
userHandler = getUserHandler :<|> registerUserHandler
  where
    getUserHandler userId = do
      res <- getUser userId
      case res of
        Just user -> return user
        Nothing   -> error "user not found"

    registerUserHandler =
      registerUser . registrationName


messagesHandler :: ServerT MessageAPI ChatM
messagesHandler sendMsg = do
  foundUser <- getUser (view sendSender sendMsg)
  case foundUser of
    Just user -> do
      liftIO $ print sendMsg
      return NoContent
    Nothing ->
      error "unknown user"


chatMToHandler :: Environment -> ChatM :~> Handler
chatMToHandler env = NT (liftIO . flip R.runReaderT env)


----------------------------------------------------------------------
-- monad stack

type ChatM = R.ReaderT Environment IO


getUser :: UserId -> ChatM (Maybe User)
getUser userId = readRegisteredUsers (view $ userFromId . at userId)


registerUser :: UserName -> ChatM UserId
registerUser name = do
  alreadyRegistered <- readRegisteredUsers (isJust . view (userIdFromName . at name))
  if alreadyRegistered
    then error "there is already a user with this name registered"
    else do
      newId <- liftIO newUserId
      let user = User newId name
      modifyRegisteredUsers (set (userFromId . at newId) (Just user) . set (userIdFromName . at name) (Just newId))
      return newId


readRegisteredUsers :: (Users -> a) -> ChatM a
readRegisteredUsers f =
  liftSTM =<< R.asks (\ env -> f <$> SV.readTVar (registeredUsers env))


modifyRegisteredUsers :: (Users -> Users) -> ChatM ()
modifyRegisteredUsers modify =
  liftSTM =<< R.asks (\ env -> SV.modifyTVar (registeredUsers env) modify)


liftSTM :: STM a -> ChatM a
liftSTM m = liftIO (atomically m)
