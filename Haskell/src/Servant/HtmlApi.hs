{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.HtmlApi
  ( API
  , handler
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Lucid
import Lucid (Html)
import Servant
import Servant.HTML.Lucid (HTML)


type API = Get '[HTML] (Html ()) :<|> Raw

----------------------------------------------------------------------
-- Servant-Handler

handler :: ServerT API Handler
handler = indexHandler :<|> serveStaticFiles


indexHandler :: Handler (Html ())
indexHandler = liftIO $ do
  index <- BS.readFile "static/index.html"
  return $ Lucid.toHtmlRaw index

serveStaticFiles :: Tagged Handler Application
serveStaticFiles = serveDirectoryWebApp "static"
