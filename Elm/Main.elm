module Main exposing (..)

import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Api.Chat exposing (UserId, User, ReceivedMessage)
import Http


baseUrl : String
baseUrl =
    "http://localhost:8081"


wsUrl : String
wsUrl =
    "ws://localhost:8081"


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
    , user : Maybe User
    , view : View
    }


type View
    = Login LoginModel
    | Chat ChatModel


type alias LoginModel =
    { name : String
    , password : String
    }


type alias ChatModel =
    { input : String
    , messages : List ChatMessage
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
    , user = Nothing
    , view = initLogin
    }
        ! []


initLogin : View
initLogin =
    Login { name = "", password = "" }


initChatModel : ChatModel
initChatModel =
    { input = "", messages = [] }


initChat : View
initChat =
    Chat initChatModel


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.view of
        Chat chatModel ->
            model.user
                |> Maybe.map (.id >> Api.Chat.webSocketSubscription MessageReceived wsUrl)
                |> Maybe.withDefault Sub.none

        _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DismissError ->
            { model | error = Nothing } ! []

        Logout ->
            { model
                | user = Nothing
                , view = initLogin
            }
                ! []

        UserInfoResponse (Ok user) ->
            { model
                | user = Just user
                , view = initChat
            }
                ! []

        UserInfoResponse (Err err) ->
            { model | error = Just (toString err) }
                ! []

        _ ->
            case model.view of
                Login loginModel ->
                    updateLogin msg loginModel model

                Chat chatModel ->
                    updateChat msg chatModel model


updateLogin : Msg -> LoginModel -> Model -> ( Model, Cmd Msg )
updateLogin msg loginModel model =
    case msg of
        SubmitLogin ->
            let
                cmd =
                    Http.send SubmitLoginResponse (Api.Chat.loginRequest baseUrl loginModel)
            in
                { model
                    | user = Nothing
                    , error = Nothing
                }
                    ! [ cmd ]

        SubmitLoginResponse (Ok userId) ->
            let
                cmd =
                    Http.send UserInfoResponse (Api.Chat.userInfoRequest baseUrl userId)
            in
                model ! [ cmd ]

        SubmitLoginResponse (Err err) ->
            { loginModel | password = "" }
                ! []
                |> setView Login { model | error = Just (toString err) }

        InputLoginName n ->
            { loginModel | name = n }
                ! []
                |> setView Login model

        InputLoginPassword p ->
            { loginModel | password = p }
                ! []
                |> setView Login model

        _ ->
            model ! []


updateChat : Msg -> ChatModel -> Model -> ( Model, Cmd Msg )
updateChat msg chatModel model =
    case msg of
        InputMessage inp ->
            { chatModel | input = inp }
                ! []
                |> setView Chat model

        SendMessage ->
            let
                cmd =
                    model.user
                        |> Maybe.map (\user -> Http.send SendMessageResponse (Api.Chat.postMessage baseUrl user.id chatModel.input))
                        |> Maybe.withDefault Cmd.none

                newMessages =
                    model.user
                        |> Maybe.map (\user -> ChatMessage user.name chatModel.input True :: chatModel.messages)
                        |> Maybe.withDefault chatModel.messages
            in
                { chatModel
                    | input = ""
                    , messages = newMessages
                }
                    ! [ cmd ]
                    |> setView Chat model

        SendMessageResponse (Ok ()) ->
            model ! []

        SendMessageResponse (Err err) ->
            { model | error = Just (toString err) }
                ! []

        MessageReceived (Ok msg) ->
            let
                newMsgs =
                    ChatMessage msg.sender msg.message False :: chatModel.messages
            in
                { chatModel | messages = newMsgs }
                    ! []
                    |> setView Chat model

        MessageReceived (Err err) ->
            { model | error = Just err }
                ! []

        _ ->
            model ! []


view : Model -> Html Msg
view model =
    let
        navbarContent =
            case model.view of
                Login loginModel ->
                    [ viewLogin loginModel ]

                Chat _ ->
                    viewUser model.user

        content =
            case model.view of
                Login _ ->
                    viewChat model initChatModel

                Chat chatModel ->
                    viewChat model chatModel
    in
        H.div
            []
            [ viewNavbar navbarContent
            , H.div
                [ Attr.class "container-fluid scrollable"
                ]
                [ viewError model.error
                , content
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
            [ H.text "Lambda-Chat" ]
         ]
            ++ userContent
        )


viewLogin : LoginModel -> Html Msg
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


viewUser : Maybe User -> List (Html Msg)
viewUser user =
    case user of
        Nothing ->
            []

        Just user ->
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


viewChat : Model -> ChatModel -> Html Msg
viewChat model chatModel =
    H.div
        []
        (viewInput model chatModel :: viewMessages chatModel.messages)


viewInput : Model -> ChatModel -> Html Msg
viewInput model chatModel =
    let
        isDisabled =
            case model.user of
                Just _ ->
                    False

                Nothing ->
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
                            , Attr.value chatModel.input
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


setView : (a -> View) -> Model -> ( a, cmd ) -> ( Model, cmd )
setView wrap model ( viewModel, cmd ) =
    ( { model | view = wrap viewModel }, cmd )
