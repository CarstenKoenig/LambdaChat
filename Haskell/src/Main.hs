{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM.TChan as SC
import qualified Control.Concurrent.STM.TVar as SV
import Control.Lens (view, set, at, makeLenses, (^.))
import Data.Maybe (fromJust)
import Control.Monad (forever)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader as R
import Data.Aeson (ToJSON, FromJSON, encode)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as URnd
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets.Connection (Connection)
import qualified Network.WebSockets.Connection as WS
import Servant
import Servant.API.WebSocket
import Servant.Server (err401, err404)

----------------------------------------------------------------------
-- User Data

type UserId = UUID
type UserName = Text

data User = User
  { _userId       :: UserId
  , _userName     :: UserName
  , _userPassword :: Text
  } deriving (Eq, Show, Generic)

makeLenses ''User


newUserId :: IO UserId
newUserId = URnd.nextRandom


data Users = Users
  { _userFromId     :: Map UserId User
  , _userIdFromName :: Map UserName UserId
  }
makeLenses ''Users


newtype UserInfo = UserInfo { username :: UserName }
  deriving (Eq, Show, Generic)

instance ToJSON UserInfo

----------------------------------------------------------------------
-- Login Data

data Login = Login
  { loginName     :: UserName
  , loginPassword :: Text
  } deriving (Eq, Show, Generic)


instance FromJSON Login


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


data WhisperMessage = WhisperMessage
  { _whispSender   :: UserId
  , _whispReceiver :: UserName
  , _whispText     :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON WhisperMessage

makeLenses ''WhisperMessage


----------------------------------------------------------------------
-- global state

data Environment = Environment
  { registeredUsers :: SV.TVar Users
  , messageChannel  :: SC.TChan ChanMessage
  }


newEnv :: IO Environment
newEnv = do
  regUsers <- SV.newTVarIO (Users Map.empty Map.empty)
  chan <- SC.newBroadcastTChanIO
  return $ Environment regUsers chan


data ChanMessage
  = Broadcast UserName Text
  | Whisper UserId UserName Text


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
  (Capture "userid" UserId :> Get '[JSON] UserInfo
   :<|> "login" :> ReqBody '[JSON] Login :> Post '[JSON] UserId
  )


type MessageAPI =
  "messages" :>
  (ReqBody '[JSON] SendMessage :> PostNoContent '[JSON] NoContent
   :<|> "whisper" :> ReqBody '[JSON] WhisperMessage :> PostNoContent '[JSON] NoContent
   :<|> Capture "userid" UserId :> WebSocket
  )


----------------------------------------------------------------------
-- Servant-Handler

userHandler :: ServerT UserAPI ChatM
userHandler = getUserHandler :<|> loginHandler
  where
    getUserHandler uId = do
      res <- getUser uId
      case res of
        Just user -> return $ UserInfo (user^.userName)
        Nothing   -> throwError $ err404 { errBody = "user not found" }

    loginHandler reg =
      loginUser (loginName reg) (loginPassword reg)


messagesHandler :: ServerT MessageAPI ChatM
messagesHandler = messageReceivedHandler :<|> whisperReceiveHandler :<|> messageWebsocketHandler


messageReceivedHandler :: SendMessage -> ChatM NoContent
messageReceivedHandler sendMsg = do
  foundUser <- getUser (sendMsg^.sendSender)
  case foundUser of
    Just user -> do
      broadcastMessage (user^.userName) (sendMsg^.sendText)
      return NoContent
    Nothing ->
      throwError $ err404 { errBody = "user not found" }


whisperReceiveHandler :: WhisperMessage -> ChatM NoContent
whisperReceiveHandler sendMsg = do
  foundSender <- getUser (sendMsg^.whispSender)
  foundReceiverId <- getUserId (sendMsg^.whispReceiver)
  case (foundSender, foundReceiverId) of
    (Just sender, Just receiverId) -> do
      whisperMessage receiverId (sender^.userName) (sendMsg^.whispText)
      return NoContent
    _ ->
      throwError $ err404 { errBody = "unknown sender or receiver" }


messageWebsocketHandler :: UserId -> Connection -> ChatM ()
messageWebsocketHandler uId connection = do
  uName <- fromJust . fmap (view userName) <$> getUser uId
  liftIO $ WS.forkPingThread connection 10
  broadcastChan <- R.asks messageChannel
  chan <- liftSTM $ SC.dupTChan broadcastChan
  liftIO $ forever $ do
    msg <- atomically $ SC.readTChan chan
    case msg of
      Broadcast senderName text
        | senderName /= uName ->
          WS.sendTextData connection (encode $ Message senderName text)
        | otherwise -> pure ()
      Whisper receiverId senderName text
        | receiverId == uId && senderName /= uName ->
          WS.sendTextData connection (encode $ Message senderName text)
        | otherwise -> pure ()


chatMToHandler :: Environment -> ChatM :~> Handler
chatMToHandler env = NT (`R.runReaderT` env)


----------------------------------------------------------------------
-- monad stack

type ChatM = R.ReaderT Environment Handler


getUser :: UserId -> ChatM (Maybe User)
getUser uId = readRegisteredUsers (view $ userFromId . at uId)


getUserId :: UserName -> ChatM (Maybe UserId)
getUserId user = readRegisteredUsers (view $ userIdFromName . at user)


loginUser :: UserName -> Text -> ChatM UserId
loginUser name password = do
  alreadyRegisteredId <- readRegisteredUsers (view (userIdFromName . at name))
  alreadyRegistered <- maybe (return Nothing) getUser alreadyRegisteredId
  case alreadyRegistered of
    Nothing -> do
      newId <- liftIO newUserId
      let user = User newId name password
      modifyRegisteredUsers (set (userFromId . at newId) (Just user) . set (userIdFromName . at name) (Just newId))
      return newId
    Just found
      | found^.userPassword == password ->
          return $ found^.userId
      | otherwise ->
        throwError $ err401 { errBody = "there is already a user with this name logged in and you don't know his password" }


broadcastMessage :: UserName -> Text -> ChatM ()
broadcastMessage senderName text =
  liftSTM =<< R.asks (\ env -> SC.writeTChan (messageChannel env) (Broadcast senderName text))


whisperMessage :: UserId -> UserName -> Text -> ChatM ()
whisperMessage receiverId senderName text =
  liftSTM =<< R.asks (\ env -> SC.writeTChan (messageChannel env) (Whisper receiverId senderName text))


readRegisteredUsers :: (Users -> a) -> ChatM a
readRegisteredUsers f =
  liftSTM =<< R.asks (\ env -> f <$> SV.readTVar (registeredUsers env))


modifyRegisteredUsers :: (Users -> Users) -> ChatM ()
modifyRegisteredUsers modify =
  liftSTM =<< R.asks (\ env -> SV.modifyTVar (registeredUsers env) modify)


liftSTM :: STM a -> ChatM a
liftSTM m = liftIO (atomically m)
