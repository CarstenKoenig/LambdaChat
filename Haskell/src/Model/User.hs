{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.User where

import Control.Lens (makeLenses, mapped, (&), (?~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, toJSON)
import Data.Maybe (fromJust)
import qualified Data.Swagger as Sw
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
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
  , _userIsOnline     :: Bool
  } deriving (Eq, Show, Generic)

makeLenses ''User


newUserId :: MonadIO m => m UserId
newUserId = liftIO URnd.nextRandom

exampleId :: UserId
exampleId = fromJust $ UUID.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

data PublicInfo = PublicInfo { username :: UserName, isonline :: Bool }
  deriving (Eq, Show, Generic)

instance ToJSON PublicInfo


publicInfo :: User -> PublicInfo
publicInfo u = PublicInfo (_userName u) (_userIsOnline u)


instance Sw.ToSchema PublicInfo where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "Userinfo example"
    & mapped.Sw.schema.Sw.example ?~ toJSON (PublicInfo "Max Mustermann" True)


