{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Messages where

import Control.Lens (mapped, (&), (?~), makeLenses)
import Data.Aeson (ToJSON, FromJSON, toJSON)
import qualified Data.Swagger as Sw
import Data.Text (Text)
import Data.Time (UTCTime(..), fromGregorian)
import GHC.Generics (Generic)
import qualified Model.Markdown as MD
import qualified Model.User as U

----------------------------------------------------------------------
-- Messages

type MessageId = Int

data MessageData
  = Post
    { _msgSender   :: U.UserName
    , _msgText     :: MD.Markdown
    , _msgHtmlBody :: Text
    , _msgPrivate  :: Bool
    }
  | System
    { _sysBody     :: Text
    }
  deriving (Eq, Show, Read, Generic)

instance ToJSON MessageData

makeLenses ''MessageData


data Message = Message
  { _msgId       :: MessageId
  , _msgTime     :: UTCTime
  , _msgData     :: MessageData
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Message

makeLenses ''Message


createMessage :: MessageId -> UTCTime -> MessageData -> Message
createMessage = Message


createPost :: UTCTime -> U.UserName -> MD.Markdown -> Bool -> MessageId -> Message
createPost time userN mdText isPrivate mId =
  let htmlBod = MD.renderHtml mdText
  in createMessage mId time (Post userN mdText htmlBod isPrivate)


createSystem :: UTCTime -> MD.Markdown -> MessageId -> Message
createSystem time mdText mId =
  let htmlBod = MD.renderHtml mdText
  in createMessage mId time (System htmlBod)


data SendMessage = SendMessage
  { _sendSender :: U.UserId
  , _sendText   :: MD.Markdown
  } deriving (Eq, Show, Generic)

instance FromJSON SendMessage

makeLenses ''SendMessage


data WhisperMessage = WhisperMessage
  { _whispSender   :: U.UserId
  , _whispReceiver :: U.UserName
  , _whispText     :: MD.Markdown
  } deriving (Eq, Show, Generic)

instance FromJSON WhisperMessage

makeLenses ''WhisperMessage


instance Sw.ToSchema MessageData where
  declareNamedSchema proxy =
    let time = UTCTime (fromGregorian 2018 1 1) 0
    in Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
       & mapped.Sw.schema.Sw.description ?~ "messages returned by the server"
       & mapped.Sw.schema.Sw.example ?~ toJSON (createPost time "Sendername" "message-text (raw)" False 42)


instance Sw.ToSchema Message where
  declareNamedSchema proxy =
    let dta = Post "message Sender" "raw text" "html representation of the markdown raw text" False
    in Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
       & mapped.Sw.schema.Sw.description ?~ "message data returned by the server"
       & mapped.Sw.schema.Sw.example ?~ toJSON dta


instance Sw.ToSchema SendMessage where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "This is how you would share your wisdom"


instance Sw.ToSchema WhisperMessage where
  declareNamedSchema proxy = Sw.genericDeclareNamedSchema Sw.defaultSchemaOptions proxy
    & mapped.Sw.schema.Sw.description ?~ "This is how your mother would send you advice"
