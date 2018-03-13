{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.User where

import           Control.Lens (makeLenses, mapped, (&), (?~))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (FromJSON (parseJSON), ToJSON, toJSON)
import           Data.Maybe (fromJust)
import qualified Data.Swagger as Sw
import           Data.Text (Text)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as URnd
import           GHC.Generics (Generic)
import           Web.HttpApiData (FromHttpApiData)
import           Data.Proxy (Proxy (Proxy))

----------------------------------------------------------------------
-- User Data

newtype UserId = UserId UUID
  deriving (Ord, Eq, Show, Read, Generic, FromHttpApiData)

instance FromJSON UserId where
  parseJSON val = UserId <$> parseJSON val

instance ToJSON UserId where
  toJSON (UserId uuid) = toJSON uuid

instance Sw.ToSchema UserId where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "uuid for an user"

instance Sw.ToParamSchema UserId where
  toParamSchema _ = Sw.toParamSchema (Proxy :: Proxy UUID)


type UserName = Text

data User = User
  { _userId       :: UserId
  , _userName     :: UserName
  , _userPassword :: Text
  , _userIsOnline     :: Bool
  } deriving (Eq, Show, Read, Generic)

makeLenses ''User


newUserId :: MonadIO m => m UserId
newUserId = liftIO $ UserId <$> URnd.nextRandom

exampleId :: UserId
exampleId = fromJust $ UserId <$> UUID.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

data PublicInfo = PublicInfo { username :: UserName, isonline :: Bool }
  deriving (Eq, Show, Generic)

instance ToJSON PublicInfo


publicInfo :: User -> PublicInfo
publicInfo u = PublicInfo (_userName u) (_userIsOnline u)


instance Sw.ToSchema PublicInfo where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "Userinfo example"
    & mapped.Sw.schema.Sw.example ?~ toJSON (PublicInfo "Max Mustermann" True)


