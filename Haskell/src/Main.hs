{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import qualified Channel as Ch
import Control.Lens (at, (^.), (&), (.~), (?~))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader as R
import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import Data.Swagger (Swagger)
import qualified Data.Swagger as Sw
import Handler
import qualified Lucid
import Lucid (Html)
import qualified Model.Login as L
import qualified Model.Messages as Msgs
import qualified Model.User as U
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import Network.WebSockets.Connection (Connection)
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid (HTML)
import Servant.Server (err404)
import Servant.Swagger
import qualified State as S
import System.Environment (lookupEnv)

----------------------------------------------------------------------
-- entry point

main :: IO ()
main = do
  handle <- S.initialize

  port <- maybe 8081 read <$> lookupEnv "PORT"

  putStrLn $ "serving app on http://localhost:" ++ show port
  Warp.run port $ Cors.cors (const $ Just policy) $ servantApp handle
  where
    policy = Cors.simpleCorsResourcePolicy
        { Cors.corsRequestHeaders = ["Content-Type"]
        , Cors.corsMethods = "POST" : Cors.simpleMethods }


servantApp :: S.Handle -> Application
servantApp env =
  serve
    (Proxy :: Proxy (API :<|> SwaggerAPI :<|> HtmlAPI)) $
    enter (toServantHandler env) (userHandler :<|> messagesHandler) :<|> pure swaggerHandler :<|> indexHandler :<|> serveStaticFiles

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

userHandler :: ServerT UserAPI ChatHandler
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


messagesHandler :: ServerT MessageAPI ChatHandler
messagesHandler = messageReceivedHandler :<|> whisperReceiveHandler :<|> messageWebsocketHandler


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


indexHandler :: Handler (Html ())
indexHandler = liftIO $ do
  index <- BS.readFile "static/index.html"
  return $ Lucid.toHtmlRaw index

serveStaticFiles :: Tagged Handler Application
serveStaticFiles = serveDirectoryWebApp "static"


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
