module Api.Chat exposing (Login, User, UserId, MessageId, ReceivedMessage, Data(..), PostData, loginRequest, logoutRequest, userInfoRequest, getMessages, postMessage, webSocketSubscription)

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
    , isOnline : Bool
    }


type UserId
    = UserId String


type alias MessageId =
    Int


type alias ReceivedMessage =
    { messageNo : MessageId
    , time : Date
    , data : Data
    }


type alias PostData =
    { sender : String
    , isPrivate : Bool
    , htmlBody : String
    , textBody : String
    }


type alias SystemData =
    { htmlBody : String
    }


type Data
    = Post PostData
    | System SystemData


userInfoRequest : String -> UserId -> Http.Request User
userInfoRequest baseUrl (UserId id) =
    let
        userDecoder =
            Json.map2 (User (UserId id))
                (Json.field "username" Json.string)
                (Json.field "isonline" Json.bool)
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


logoutRequest : String -> UserId -> Http.Request ()
logoutRequest baseUrl (UserId id) =
    let
        logoutBody =
            Enc.object
                [ ( "userId", Enc.string id ) ]
    in
        Http.request
            { method = "POST"
            , headers = []
            , url = baseUrl ++ "/users/logout"
            , body = Http.jsonBody logoutBody
            , expect = Http.expectStringResponse (always <| Ok ())
            , timeout = Nothing
            , withCredentials = False
            }


getMessages : String -> Maybe UserId -> Maybe Int -> Http.Request (List ReceivedMessage)
getMessages baseUrl userOpt fromMsgNo =
    let
        query =
            Maybe.map (\no -> "?fromid=" ++ toString no) fromMsgNo
                |> Maybe.withDefault ""

        uri =
            case userOpt of
                Just (UserId id) ->
                    baseUrl ++ "/messages/" ++ id ++ query

                Nothing ->
                    baseUrl ++ "/messages/public" ++ query
    in
        Http.get uri (Json.list messageDecoder)


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


webSocketSubscription : (Result String ReceivedMessage -> msg) -> String -> Maybe UserId -> Sub msg
webSocketSubscription toMsg baseUrl userOpt =
    let
        decode =
            Json.decodeString messageDecoder >> toMsg
    in
        case userOpt of
            Just (UserId id) ->
                Ws.listen (baseUrl ++ "/messages/stream/" ++ id) decode

            Nothing ->
                Ws.listen (baseUrl ++ "/messages/stream/public") decode


messageDecoder : Json.Decoder ReceivedMessage
messageDecoder =
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
                [ Json.map
                    SystemData
                    (Json.field "_sysBody" Json.string)
                    |> Json.map System
                , Json.map4
                    PostData
                    (Json.field "_msgSender" Json.string)
                    (Json.field "_msgPrivate" Json.bool)
                    (Json.field "_msgHtmlBody" Json.string)
                    (Json.field "_msgText" Json.string)
                    |> Json.map Post
                ]
    in
        Json.map3 ReceivedMessage
            (Json.field "_msgId" Json.int)
            (Json.field "_msgTime" decodeDate)
            (Json.field "_msgData" decodeData)
