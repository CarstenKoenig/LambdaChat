module Api.Chat exposing (Login, User, UserId, ReceivedMessage, Data(..), PostData, loginRequest, userInfoRequest, postMessage, webSocketSubscription)

import Http
import Json.Decode as Json
import Json.Encode as Enc
import WebSocket as Ws
import Date exposing (Date)


type alias Login r =
    { r
        | name : String
        , password : String
    }


type alias User =
    { id : UserId
    , name : String
    }


type UserId
    = UserId String


type alias ReceivedMessage =
    { messageNo : Int
    , time : Date
    , data : Data
    }


type alias PostData =
    { sender : String
    , isPrivate : Bool
    , htmlBody : String
    }


type Data
    = Post PostData


userInfoRequest : String -> UserId -> Http.Request User
userInfoRequest baseUrl (UserId id) =
    let
        userDecoder =
            Json.map (User <| UserId id) (Json.field "username" Json.string)
    in
        Http.get (baseUrl ++ "/users/" ++ id) userDecoder


loginRequest : String -> Login r -> Http.Request UserId
loginRequest baseUrl login =
    let
        loginBody =
            Enc.object
                [ ( "loginName", Enc.string login.name )
                , ( "loginPassword", Enc.string login.password )
                ]
    in
        Http.post (baseUrl ++ "/users/login") (Http.jsonBody <| loginBody) (Json.string |> Json.map UserId)


postMessage : String -> UserId -> String -> Http.Request ()
postMessage baseUrl (UserId id) message =
    let
        msg =
            Enc.object
                [ ( "_sendSender", Enc.string id )
                , ( "_sendText", Enc.string message )
                ]
    in
        Http.request
            { method = "POST"
            , headers = []
            , url = baseUrl ++ "/messages"
            , body = Http.jsonBody msg
            , expect = Http.expectStringResponse (always <| Ok ())
            , timeout = Nothing
            , withCredentials = False
            }


webSocketSubscription : (Result String ReceivedMessage -> msg) -> String -> UserId -> Sub msg
webSocketSubscription toMsg baseUrl (UserId id) =
    let
        decodeDate =
            Json.string
                |> Json.andThen
                    (\dateStr ->
                        case Date.fromString dateStr of
                            Ok date ->
                                Json.succeed date

                            Err err ->
                                Json.fail err
                    )

        decodeData =
            Json.oneOf
                [ Json.map3
                    PostData
                    (Json.field "_msgSender" Json.string)
                    (Json.field "_msgPrivate" Json.bool)
                    (Json.field "_msgHtmlBody" Json.string)
                    |> Json.map Post
                ]

        decoder =
            Json.map3 ReceivedMessage
                (Json.field "_msgId" Json.int)
                (Json.field "_msgTime" decodeDate)
                (Json.field "_msgData" decodeData)

        decode =
            Json.decodeString decoder >> toMsg
    in
        Ws.listen (baseUrl ++ "/messages/" ++ id) decode
