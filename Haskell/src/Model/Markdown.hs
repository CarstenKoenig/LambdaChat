{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Markdown
  ( Markdown
  , fromText
  , renderHtml
  , cssStyles
  )where

import Data.Aeson (ToJSON, FromJSON)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString)
import Data.Swagger (ToSchema, declareNamedSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Skylighting ( TokenizerConfig(..), SourceLine)
import qualified Skylighting as Sky
import qualified Text.Blaze.Html as TBH
import qualified Text.Blaze.Html.Renderer.Text as TBH
import Text.Markdown (MarkdownSettings(..), markdown)

newtype Markdown = Markdown Text
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON, IsString)


fromText :: Text -> Markdown
fromText = Markdown


-- | should be used to export a .css file for the syntax-highlighting
-- use `writeFile "kate.css" (T.unpack cssStyles)
cssStyles :: Text
cssStyles = T.pack $ Sky.styleToCss Sky.pygments

----------------------------------------------------------------------
-- markdown-to HTML

renderHtml :: Markdown -> Text
renderHtml (Markdown mdText) =
  TL.toStrict . TBH.renderHtml $ markdown markdownDef (TL.fromStrict mdText)
  where
    markdownDef = def
      { msBlockCodeRenderer = renderer
      , msXssProtect        = True
      }
    renderer lang (src,_) = renderLang lang src


renderLang :: Maybe Text -> Text -> TBH.Html
renderLang lang src =
  Sky.formatHtmlBlock Sky.defaultFormatOpts
  $ highlightAs (fromMaybe "haskell" lang) src


highlightAs :: Text -> Text -> [SourceLine]
highlightAs lang source =
  case Sky.lookupSyntax lang Sky.defaultSyntaxMap of
    Nothing -> highlightAs "haskell" source
    Just syntax ->
      case Sky.tokenize config syntax source of
        Left _ -> []
        Right ls -> ls
  where
    config = TokenizerConfig Sky.defaultSyntaxMap False


instance ToSchema Markdown where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
