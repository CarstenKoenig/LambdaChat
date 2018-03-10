{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.UserApi
  ( API
  , handler
  ) where

import           Control.Monad.Except (throwError)
import qualified Data.Text as T
import qualified Model.Login as L
import qualified Model.Markdown as MD
import qualified Model.User as U
import           Servant
import           Servant.Handler
import           Servant.Server (err404)
import qualified State as S

type API =
  "users" :>
  (Get '[JSON] [U.PublicInfo]
   :<|> Capture "userid" U.UserId :> Get '[JSON] U.PublicInfo
   :<|> "login" :> ReqBody '[JSON] L.Login :> Post '[JSON] U.UserId
   :<|> "logout" :> ReqBody '[JSON] L.Logout :> PostNoContent '[JSON] NoContent
  )


----------------------------------------------------------------------
-- Servant-Handler

handler :: S.Handle -> ServerT API Handler
handler handle =
  enter (toServantHandler handle) $ allUsers :<|> getUserHandler :<|> loginHandler :<|> logoutHandler

  where
    allUsers =
      listAllUsers

    getUserHandler uId = do
      res <- getUser uId
      case res of
        Just user -> return $ U.publicInfo user
        Nothing   -> throwError $ err404 { errBody = "user not found" }

    loginHandler reg = do
      res <- loginUser (L.loginName reg) (L.loginPassword reg)
      systemMessage $ MD.fromText $ T.pack $ "user **" ++ T.unpack (L.loginName reg) ++ "** connected"
      return res


    logoutHandler lout =
      logoutUser (L.userId lout)
