{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import           Control.Lens (at, (&), (.~), (?~))
import           Data.Proxy (Proxy(..))
import           Data.Swagger (Swagger)
import qualified Data.Swagger as Sw
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import           Servant
import           Servant.API.WebSocket
import qualified Servant.HtmlApi as Hapi
import qualified Servant.MessageApi as Mapi
import           Servant.Swagger
import qualified Servant.UserApi as Uapi
import qualified State as S
import           System.Environment (lookupEnv)

----------------------------------------------------------------------
-- entry point

main :: IO ()
main = do
  -- initialize runtime with cache-size of 10 messages
  handle <- S.initialize 10

  port <- maybe 8081 read <$> lookupEnv "PORT"

  putStrLn $ "serving app on http://localhost:" ++ show port
  Warp.run port $ Cors.cors (const $ Just policy) $ servantApp handle

  where
    policy = Cors.simpleCorsResourcePolicy
        { Cors.corsRequestHeaders = ["Content-Type"]
        , Cors.corsMethods = "POST" : Cors.simpleMethods }


----------------------------------------------------------------------
-- Servant

type API = Uapi.API :<|> Mapi.API


servantApp :: S.Handle -> Application
servantApp h =
  serve
    (Proxy :: Proxy (API :<|> SwaggerAPI :<|> Hapi.API)) $ 
    (Uapi.handler h :<|> Mapi.handler h) :<|> pure swaggerHandler :<|> Hapi.handler


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
