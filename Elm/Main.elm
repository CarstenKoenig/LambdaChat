module Main exposing (..)

import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Api.Chat exposing (UserId, User, ReceivedMessage)
import Http


baseUrl : String
baseUrl =
    "http://localhost:80"


wsUrl : String
wsUrl =
    "ws://localhost:80"


main : Program Never Model Msg
main =
    H.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { error : Maybe String
    , login : LoginModel
    , messageInput : String
    , messages : List ChatMessage
    }


type LoginModel
    = LoggedIn User
    | Login
        { name : String
        , password : String
        }


type alias ChatMessage =
    { sender : String
    , message : String
    , ownMessage : Bool
    }


type Msg
    = SubmitLogin
    | SubmitLoginResponse (Result Http.Error UserId)
    | UserInfoResponse (Result Http.Error User)
    | Logout
    | InputLoginName String
    | InputLoginPassword String
    | InputMessage String
    | SendMessage
    | SendMessageResponse (Result Http.Error ())
    | MessageReceived (Result String Api.Chat.ReceivedMessage)
    | DismissError


init : ( Model, Cmd Msg )
init =
    { error = Nothing
    , login = Login { name = "", password = "" }
    , messageInput = ""
    , messages = []
    }
        ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.login of
        LoggedIn user ->
            user.id
                |> Api.Chat.webSocketSubscription MessageReceived wsUrl

        _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DismissError ->
            { model | error = Nothing } ! []

        --- login/logout
        InputLoginName n ->
            case model.login of
                Login loginModel ->
                    { model | login = Login { loginModel | name = n } } ! []

                LoggedIn _ ->
                    model ! []

        InputLoginPassword p ->
            case model.login of
                Login loginModel ->
                    { model | login = Login { loginModel | password = p } } ! []

                LoggedIn _ ->
                    model ! []

        Logout ->
            { model | login = Login { name = "", password = "" } } ! []

        SubmitLogin ->
            let
                cmd =
                    case model.login of
                        Login loginModel ->
                            Http.send SubmitLoginResponse (Api.Chat.loginRequest baseUrl loginModel)

                        LoggedIn _ ->
                            Cmd.none
            in
                { model | error = Nothing } ! [ cmd ]

        SubmitLoginResponse (Ok userId) ->
            let
                cmd =
                    Http.send UserInfoResponse (Api.Chat.userInfoRequest baseUrl userId)
            in
                model ! [ cmd ]

        SubmitLoginResponse (Err err) ->
            case model.login of
                Login loginModel ->
                    { model | login = Login { loginModel | password = "" } } ! []

                LoggedIn _ ->
                    model ! []

        UserInfoResponse (Ok user) ->
            { model | login = LoggedIn user } ! []

        UserInfoResponse (Err err) ->
            { model | error = Just (toString err) }
                ! []

        InputMessage inp ->
            { model | messageInput = inp }
                ! []

        --- message sending/receiving
        SendMessage ->
            let
                cmd =
                    case model.login of
                        LoggedIn user ->
                            Http.send SendMessageResponse (Api.Chat.postMessage baseUrl user.id model.messageInput)

                        Login _ ->
                            Cmd.none

                newMessages =
                    case model.login of
                        LoggedIn user ->
                            ChatMessage user.name model.messageInput True :: model.messages

                        Login _ ->
                            model.messages
            in
                { model
                    | messageInput = ""
                    , messages = newMessages
                }
                    ! [ cmd ]

        SendMessageResponse (Ok ()) ->
            model ! []

        SendMessageResponse (Err err) ->
            { model | error = Just (toString err) }
                ! []

        MessageReceived (Ok msg) ->
            let
                newMsgs =
                    ChatMessage msg.sender msg.message False :: model.messages
            in
                { model | messages = newMsgs }
                    ! []

        MessageReceived (Err err) ->
            { model | error = Just err }
                ! []


view : Model -> Html Msg
view model =
    let
        navbarContent =
            case model.login of
                Login loginModel ->
                    [ viewLogin loginModel ]

                LoggedIn user ->
                    viewUser user
    in
        H.div
            []
            [ viewNavbar navbarContent
            , H.div
                [ Attr.class "container scrollable"
                ]
                [ viewError model.error
                , viewChat model
                ]
            ]


viewError : Maybe String -> Html Msg
viewError err =
    case err of
        Nothing ->
            H.text ""

        Just text ->
            H.div
                [ Attr.class "alert alert-danger", Attr.attribute "role" "alert", Ev.onClick DismissError ]
                [ H.p [] [ H.strong [] [ H.text "error: " ], H.text text ] ]


viewNavbar : List (Html Msg) -> Html Msg
viewNavbar userContent =
    H.nav
        [ Attr.class "navbar navbar-light bg-light sticky-top justify-content-between" ]
        ([ H.a
            [ Attr.class "navbar-brand", Attr.href "#" ]
            [ H.text "λ Chat" ]
         ]
            ++ userContent
        )


viewLogin : { name : String, password : String } -> Html Msg
viewLogin model =
    H.form
        [ Ev.onSubmit SubmitLogin ]
        [ H.div
            [ Attr.class "form-row align-items-center" ]
            [ H.div
                [ Attr.class "col" ]
                [ H.input
                    [ Attr.type_ "text"
                    , Attr.class "form-control mb-2"
                    , Attr.placeholder "username"
                    , Attr.autocomplete False
                    , Ev.onInput InputLoginName
                    , Attr.value model.name
                    ]
                    []
                ]
            , H.div
                [ Attr.class "col" ]
                [ H.input
                    [ Attr.type_ "password"
                    , Attr.class "form-control mb-2"
                    , Attr.placeholder "password"
                    , Ev.onInput InputLoginPassword
                    , Attr.value model.password
                    ]
                    []
                ]
            , H.div
                [ Attr.class "col-auto" ]
                [ H.button
                    [ Attr.type_ "submit"
                    , Attr.class "btn btn-outline-success mb-2"
                    ]
                    [ H.text "login" ]
                ]
            ]
        ]


viewUser : User -> List (Html Msg)
viewUser user =
    [ H.span [ Attr.class "navbar-text" ] [ H.text "hello, ", H.strong [] [ H.text user.name ] ]
    , H.form
        [ Attr.class "form-inline", Ev.onSubmit Logout ]
        [ H.button
            [ Attr.type_ "submit"
            , Attr.class "btn btn-outline-danger my-2 my-sm-0"
            ]
            [ H.text "logout" ]
        ]
    ]


viewChat : Model -> Html Msg
viewChat model =
    H.div
        []
        (viewInput model :: viewMessages model.messages)


viewInput : Model -> Html Msg
viewInput model =
    let
        isDisabled =
            case model.login of
                LoggedIn _ ->
                    False

                Login _ ->
                    True
    in
        H.nav
            [ Attr.class "navbar navbar-light bg-light fixed-bottom justify-content-between" ]
            [ H.form
                (if isDisabled then
                    [ Attr.class "w-100" ]
                 else
                    [ Attr.class "w-100", Ev.onSubmit SendMessage ]
                )
                [ H.div
                    [ Attr.class "form-row align-items-center" ]
                    [ H.div
                        [ Attr.class "col" ]
                        [ H.input
                            [ Attr.type_ "text"
                            , Attr.class "form-control mb-2"
                            , Attr.placeholder "message"
                            , Ev.onInput InputMessage
                            , Attr.value model.messageInput
                            , Attr.disabled isDisabled
                            ]
                            []
                        ]
                    , H.div
                        [ Attr.class "col-auto" ]
                        [ H.button
                            [ Attr.type_ "submit"
                            , Attr.class "btn mb-2"
                            , Attr.classList
                                [ ( "btn-outline-success", not isDisabled )
                                , ( "btn-outline-danger", isDisabled )
                                ]
                            , Attr.disabled isDisabled
                            ]
                            [ H.text "send" ]
                        ]
                    ]
                ]
            ]


viewMessages : List ChatMessage -> List (Html Msg)
viewMessages =
    List.map viewMessage


viewMessage : ChatMessage -> Html Msg
viewMessage msg =
    H.div
        [ Attr.class "card w-75 mb-2"
        , Attr.classList
            [ ( "float-right", not msg.ownMessage ) ]
        ]
        [ H.div
            [ Attr.class "card-body" ]
            [ H.h5 [ Attr.class "card-title" ] [ H.text msg.sender ]
            , H.p [ Attr.class "card-text" ] [ H.text msg.message ]
            ]
        ]
