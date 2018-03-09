{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM.TChan as SC
import qualified Control.Concurrent.STM.TVar as SV
import Control.Lens (view, set, at, (^.), (&), (.~), (?~))
import Control.Monad (forever)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader as R
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))
import Data.Swagger (Swagger)
import qualified Data.Swagger as Sw
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified Lucid
import Lucid (Html)
import qualified Model.Login as L
import qualified Model.Markdown as MD
import qualified Model.Messages as Msgs
import qualified Model.User as U
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import Network.WebSockets.Connection (Connection)
import qualified Network.WebSockets.Connection as WS
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid (HTML)
import Servant.Server (err401, err404)
import Servant.Swagger
import System.Environment (lookupEnv)

----------------------------------------------------------------------
-- global state

data Environment = Environment
  { registeredUsers :: SV.TVar U.Users
  , messageChannel  :: SC.TChan ChanMessage
  }


newEnv :: IO Environment
newEnv = do
  regUsers <- SV.newTVarIO (U.Users Map.empty Map.empty)
  chan <- SC.newBroadcastTChanIO
  return $ Environment regUsers chan


data ChanMessage
  = Broadcast U.UserName MD.Markdown
  | Whisper U.UserId U.UserName MD.Markdown


----------------------------------------------------------------------
-- entry point

main :: IO ()
main = do
  env <- newEnv

  port <- maybe 8081 read <$> lookupEnv "PORT"

  putStrLn $ "serving app on http://localhost:" ++ show port
  Warp.run port $ Cors.cors (const $ Just policy) $ servantApp env
  where
    policy = Cors.simpleCorsResourcePolicy
        { Cors.corsRequestHeaders = ["Content-Type"]
        , Cors.corsMethods = "POST" : Cors.simpleMethods }


servantApp :: Environment -> Application
servantApp env =
  serve
    (Proxy :: Proxy (API :<|> SwaggerAPI :<|> HtmlAPI)) $
    enter (chatMToHandler env) (userHandler :<|> messagesHandler) :<|> pure swaggerHandler :<|> indexHandler :<|> serveStaticFiles

----------------------------------------------------------------------
-- Servant-API

type API = UserAPI :<|> MessageAPI

type UserAPI =
  "users" :>
  (Get '[JSON] [U.PublicInfo]
   :<|> Capture "userid" U.UserId :> Get '[JSON] U.PublicInfo
   :<|> "login" :> ReqBody '[JSON] L.Login :> Post '[JSON] U.UserId
  )


type MessageAPI =
  "messages" :>
  (ReqBody '[JSON] Msgs.SendMessage :> PostNoContent '[JSON] NoContent
   :<|> "whisper" :> ReqBody '[JSON] Msgs.WhisperMessage :> PostNoContent '[JSON] NoContent
   :<|> Capture "userid" U.UserId :> WebSocket
  )


type HtmlAPI = Get '[HTML] (Html ()) :<|> Raw

----------------------------------------------------------------------
-- Servant-Handler

userHandler :: ServerT UserAPI ChatM
userHandler = allUsers :<|> getUserHandler :<|> loginHandler
  where
    allUsers =
      listAllUsers
    getUserHandler uId = do
      res <- getUser uId
      case res of
        Just user -> return $ U.publicInfo user
        Nothing   -> throwError $ err404 { errBody = "user not found" }

    loginHandler reg =
      loginUser (L.loginName reg) (L.loginPassword reg)


messagesHandler :: ServerT MessageAPI ChatM
messagesHandler = messageReceivedHandler :<|> whisperReceiveHandler :<|> messageWebsocketHandler


messageReceivedHandler :: Msgs.SendMessage -> ChatM NoContent
messageReceivedHandler sendMsg = do
  foundUser <- getUser (sendMsg^.Msgs.sendSender)
  case foundUser of
    Just user -> do
      broadcastMessage (user^.U.userName) (sendMsg^.Msgs.sendText)
      return NoContent
    Nothing ->
      throwError $ err404 { errBody = "user not found" }


whisperReceiveHandler :: Msgs.WhisperMessage -> ChatM NoContent
whisperReceiveHandler sendMsg = do
  foundSender <- getUser (sendMsg^.Msgs.whispSender)
  foundReceiverId <- getUserId (sendMsg^.Msgs.whispReceiver)
  case (foundSender, foundReceiverId) of
    (Just sender, Just receiverId) -> do
      whisperMessage receiverId (sender^.U.userName) (sendMsg^.Msgs.whispText)
      return NoContent
    _ ->
      throwError $ err404 { errBody = "unknown sender or receiver" }


messageWebsocketHandler :: U.UserId -> Connection -> ChatM ()
messageWebsocketHandler uId connection = do
  uName <- fromJust . fmap (view U.userName) <$> getUser uId
  liftIO $ WS.forkPingThread connection 10
  broadcastChan <- R.asks messageChannel
  chan <- liftSTM $ SC.dupTChan broadcastChan
  liftIO $ forever $ do
    msg <- atomically $ SC.readTChan chan
    time <- liftIO getCurrentTime
    case msg of
      Broadcast senderName text ->
        WS.sendTextData connection (encode $ Msgs.Message senderName text time False $ MD.renderHtml text)
      Whisper receiverId senderName text
        | receiverId == uId && senderName /= uName ->
          WS.sendTextData connection (encode $ Msgs.Message senderName text time True $ MD.renderHtml text)
        | otherwise -> pure ()


indexHandler :: Handler (Html ())
indexHandler = liftIO $ do
  index <- BS.readFile "static/index.html"
  return $ Lucid.toHtmlRaw index

serveStaticFiles :: Tagged Handler Application
serveStaticFiles = serveDirectoryWebApp "static"


chatMToHandler :: Environment -> ChatM :~> Handler
chatMToHandler env = NT (`R.runReaderT` env)


----------------------------------------------------------------------
-- monad stack

type ChatM = R.ReaderT Environment Handler

listAllUsers :: ChatM [U.PublicInfo]
listAllUsers =
  readRegisteredUsers (fmap toInfo . Map.elems . U._userFromId)
  where toInfo user = U.publicInfo user

getUser :: U.UserId -> ChatM (Maybe U.User)
getUser uId = readRegisteredUsers (view $ U.userFromId . at uId)


getUserId :: U.UserName -> ChatM (Maybe U.UserId)
getUserId user = readRegisteredUsers (view $ U.userIdFromName . at user)


loginUser :: U.UserName -> Text -> ChatM U.UserId
loginUser name password = do
  alreadyRegisteredId <- readRegisteredUsers (view (U.userIdFromName . at name))
  alreadyRegistered <- maybe (return Nothing) getUser alreadyRegisteredId
  case alreadyRegistered of
    Nothing -> do
      newId <- liftIO U.newUserId
      let user = U.User newId name password
      modifyRegisteredUsers (set (U.userFromId . at newId) (Just user) . set (U.userIdFromName . at name) (Just newId))
      return newId
    Just found
      | found^.U.userPassword == password ->
          return $ found^.U.userId
      | otherwise ->
        throwError $ err401 { errBody = "there is already a user with this name logged in and you don't know his password" }


broadcastMessage :: U.UserName -> MD.Markdown -> ChatM ()
broadcastMessage senderName text =
  liftSTM =<< R.asks (\ env -> SC.writeTChan (messageChannel env) (Broadcast senderName text))


whisperMessage :: U.UserId -> U.UserName -> MD.Markdown -> ChatM ()
whisperMessage receiverId senderName text =
  liftSTM =<< R.asks (\ env -> SC.writeTChan (messageChannel env) (Whisper receiverId senderName text))


readRegisteredUsers :: (U.Users -> a) -> ChatM a
readRegisteredUsers f =
  liftSTM =<< R.asks (\ env -> f <$> SV.readTVar (registeredUsers env))


modifyRegisteredUsers :: (U.Users -> U.Users) -> ChatM ()
modifyRegisteredUsers modify =
  liftSTM =<< R.asks (\ env -> SV.modifyTVar (registeredUsers env) modify)


liftSTM :: STM a -> ChatM a
liftSTM m = liftIO (atomically m)


----------------------------------------------------------------------
-- swagger

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

swaggerHandler :: Swagger
swaggerHandler = toSwagger (Proxy :: Proxy API)
  & Sw.info.Sw.title   .~ "LambdaChat API"
  & Sw.info.Sw.version .~ "0.1"
  & Sw.info.Sw.description ?~ "pure Chat"
  & Sw.info.Sw.license ?~ ("MIT" & Sw.url ?~ Sw.URL "http://mit.com")


instance HasSwagger WebSocket where
  toSwagger _ = mempty & Sw.paths . at "/" ?~ mempty
