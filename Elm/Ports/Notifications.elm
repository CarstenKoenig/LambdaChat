port module Ports.Notifications exposing (..)


type alias Notification =
    { title : String
    , body : String
    }


port showNotification : Notification -> Cmd msg
