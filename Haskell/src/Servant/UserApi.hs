{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.UserApi
  ( API
  , handler
  ) where

import Control.Monad.Except (throwError)
import Servant.Handler
import qualified Model.Login as L
import qualified Model.User as U
import Servant
import Servant.Server (err404)


type API =
  "users" :>
  (Get '[JSON] [U.PublicInfo]
   :<|> Capture "userid" U.UserId :> Get '[JSON] U.PublicInfo
   :<|> "login" :> ReqBody '[JSON] L.Login :> Post '[JSON] U.UserId
  )


----------------------------------------------------------------------
-- Servant-Handler

handler :: ServerT API ChatHandler
handler = allUsers :<|> getUserHandler :<|> loginHandler
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
