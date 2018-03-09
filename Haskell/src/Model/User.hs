{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.User where

import Control.Lens (makeLenses, mapped, (&), (?~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, toJSON)
import qualified Data.Swagger as Sw
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as URnd
import GHC.Generics (Generic)

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


newUserId :: MonadIO m => m UserId
newUserId = liftIO URnd.nextRandom



newtype PublicInfo = PublicInfo { username :: UserName }
  deriving (Eq, Show, Generic)

instance ToJSON PublicInfo


publicInfo :: User -> PublicInfo
publicInfo u = PublicInfo (_userName u)


instance Sw.ToSchema PublicInfo where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "Userinfo example"
    & mapped.Sw.schema.Sw.example ?~ toJSON (PublicInfo "Max Mustermann")


