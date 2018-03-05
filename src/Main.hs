{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Arrow ((***))
import Control.Concurrent.STM (STM, atomically)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as SV
import Control.Lens (view, over, set, at, _1, _2)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Reader as R
import Data.Aeson (ToJSON)
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
-- entry point

main :: IO ()
main = do
  env <- newEnv

  putStrLn "serving app on http://localhost:8081"
  Warp.run 8081 $ servantApp env


servantApp :: Environment -> Application
servantApp env =
  serve (Proxy :: Proxy UserAPI) $ enter (chatMToHandler env) userHandler

----------------------------------------------------------------------
-- Servant-API

type UserAPI =
  "users" :>
  (Capture "userid" UserId :> Get '[JSON] User
   :<|> "register" :> QueryParam "username" UserName :> Post '[JSON] UserId)


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

    registerUserHandler (Just userName) = do
      registerUser userName


chatMToHandler :: Environment -> ChatM :~> Handler
chatMToHandler env = NT (liftIO . flip R.runReaderT env)

----------------------------------------------------------------------
-- global state

data Environment = Environment
  { registeredUsers :: SV.TVar (Map UserId User, Map UserName UserId)
  }


newEnv :: IO Environment
newEnv = do
  regUsers <- atomically (SV.newTVar (Map.empty, Map.empty))
  return $ Environment regUsers

----------------------------------------------------------------------
-- monad stack

type ChatM = R.ReaderT Environment IO


getUser :: UserId -> ChatM (Maybe User)
getUser userId = readRegisteredUsers (view $ _1 . at userId)


registerUser :: UserName -> ChatM UserId
registerUser name = do
  alreadyRegistered <- readRegisteredUsers (isJust . view (_2 . at name))
  if alreadyRegistered
    then error "there is already a user with this name registered"
    else do
      newId <- liftIO newUserId
      let user = User newId name
      modifyRegisteredUsers (Map.insert newId user) (Map.insert name newId)
      return newId


readRegisteredUsers :: ((Map UserId User, Map UserName UserId) -> a) -> ChatM a
readRegisteredUsers f =
  liftSTM =<< R.asks (\ env -> f <$> SV.readTVar (registeredUsers env))


modifyRegisteredUsers :: (Map UserId User -> Map UserId User) -> (Map UserName UserId -> Map UserName UserId) -> ChatM ()
modifyRegisteredUsers modFst modSnd =
  liftSTM =<< R.asks (\ env -> SV.modifyTVar (registeredUsers env) (modFst *** modSnd))


liftSTM :: STM a -> ChatM a
liftSTM m = liftIO (atomically m)

----------------------------------------------------------------------
-- User Data

type UserId = UUID
type UserName = Text

data User = User
  { id   :: UserId
  , name :: UserName
  } deriving (Eq, Show, Generic)


newUserId :: IO UserId
newUserId = URnd.nextRandom

instance ToJSON User
