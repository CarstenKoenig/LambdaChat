{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Login where

import Control.Lens (mapped, (&), (?~))
import Data.Aeson (ToJSON, FromJSON, toJSON)
import qualified Data.Swagger as Sw
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Model.User as U
----------------------------------------------------------------------
-- Login Data

data Login = Login
  { loginName     :: U.UserName
  , loginPassword :: Text
  } deriving (Eq, Show, Generic)


instance FromJSON Login
instance ToJSON Login


instance Sw.ToSchema Login where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "A login for a user 'Max' with password '1234' should look like this"
    & mapped.Sw.schema.Sw.example ?~ toJSON (Login "Max" "1234")


data Logout = Logout
  { userId :: U.UserId
  } deriving (Eq, Show, Generic)


instance FromJSON Logout
instance ToJSON Logout


instance Sw.ToSchema Logout where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "A logout request for a user"
    & mapped.Sw.schema.Sw.example ?~ toJSON (Logout U.exampleId)

