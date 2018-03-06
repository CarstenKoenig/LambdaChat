module Api.Chat exposing (Login, User, UserId, loginRequest, userInfoRequest, postMessage)

import Http
import Json.Decode as Json
import Json.Encode as Enc


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
