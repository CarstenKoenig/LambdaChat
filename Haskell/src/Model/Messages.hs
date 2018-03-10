{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Messages where

import Control.Lens (mapped, (&), (?~), makeLenses)
import Data.Aeson (ToJSON, FromJSON, toJSON)
import Data.Maybe (fromJust)
import qualified Data.Swagger as Sw
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import qualified Model.Markdown as MD
import qualified Model.User as U

----------------------------------------------------------------------
-- Messages

type MessageId = Int

data Message a = Message
  { _msgId       :: MessageId
  , _msgTime     :: UTCTime
  , _msgData     :: a
  } deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Message a)

makeLenses ''Message


data Post = Post
  { _msgSender   :: U.UserName
  , _msgText     :: MD.Markdown
  , _msgHtmlBody :: Text
  , _msgPrivate  :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Post

makeLenses ''Post


createMessage :: MessageId -> UTCTime -> a -> Message a
createMessage = Message


createPost :: UTCTime -> U.UserName -> MD.Markdown -> Bool -> MessageId -> Message Post
createPost time userN mdText isPrivate mId =
  let htmlBod = MD.renderHtml mdText
  in createMessage mId time (Post userN mdText htmlBod isPrivate)


data SendMessage = SendMessage
  { _sendSender :: U.UserId
  , _sendText   :: MD.Markdown
  } deriving (Eq, Show, Generic)

instance FromJSON SendMessage
instance ToJSON SendMessage

makeLenses ''SendMessage


data WhisperMessage = WhisperMessage
  { _whispSender   :: U.UserId
  , _whispReceiver :: U.UserName
  , _whispText     :: MD.Markdown
  } deriving (Eq, Show, Generic)

instance FromJSON WhisperMessage
instance ToJSON WhisperMessage

makeLenses ''WhisperMessage

instance Sw.ToSchema SendMessage where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "This is how you would share your wisdom"
    & mapped.Sw.schema.Sw.example ?~ toJSON (SendMessage (fromJust $ UUID.fromText "a90d6e28-621d-4b23-b8d7-2fc045580846") "True == False")


instance Sw.ToSchema WhisperMessage where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "This is how your mother would send you advice"
    & mapped.Sw.schema.Sw.example ?~ toJSON (WhisperMessage (fromJust $ UUID.fromText "a90d6e28-621d-4b23-b8d7-2fc045580846") "Son" "True == False")


