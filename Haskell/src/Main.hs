{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import           Control.Concurrent (myThreadId, throwTo)
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
import           System.Exit (ExitCode(ExitSuccess))
import           System.Posix.Signals (installHandler, sigTERM, sigINT, Handler(CatchOnce))

----------------------------------------------------------------------
-- entry point

main :: IO ()
main = do
  -- initialize runtime with cache-size of 50 messages (try saved state first)
  -- that are valid for 1 hour
  handle <- maybe (S.initialize 50 (60*60)) return =<< S.loadState "state.data"

  port <- maybe 80 read <$> lookupEnv "PORT"

  tid <- myThreadId
  _   <- installHandler sigTERM (CatchOnce $ saveAndExit handle tid) Nothing
  _   <- installHandler sigINT (CatchOnce $ saveAndExit handle tid) Nothing

  putStrLn $ "serving app on http://localhost:" ++ show port
  Warp.run port $ Cors.cors (const $ Just policy) $ servantApp handle

  where
    policy = Cors.simpleCorsResourcePolicy
        { Cors.corsRequestHeaders = ["Content-Type"]
        , Cors.corsMethods = "POST" : Cors.simpleMethods }
    saveAndExit handle tid = do
      putStrLn "shuting down..."
      S.saveState handle "state.data"
      throwTo tid ExitSuccess


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
