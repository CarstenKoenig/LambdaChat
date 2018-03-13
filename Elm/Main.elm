module Main exposing (..)

import Api.Chat exposing (UserId, User, MessageId, ReceivedMessage)
import Date exposing (Date)
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Keyboard as Kbd
import Markdown as MD
import Time exposing (Time)
import Dict exposing (Dict)
import Ports.Notifications as N


ctrlKeyCode : Kbd.KeyCode
ctrlKeyCode =
    17


enterKeyCode : Kbd.KeyCode
enterKeyCode =
    13


type alias Flags =
    { baseUri : String
    , wsUri : String
    }


main : Program Flags Model Msg
main =
    H.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { flags : Flags
    , error : Maybe String
    , login : LoginModel
    , messageInputFocused : Bool
    , messageInputMouseOver : Bool
    , messageInput : String
    , messages : Dict MessageId Message
    , ctrlKeyPressed : Bool
    , currentTime : Time
    }


currentUserName : Model -> Maybe String
currentUserName model =
    case model.login of
        LoggedIn user ->
            Just user.name

        Login _ ->
            Nothing


type LoginModel
    = LoggedIn User
    | Login
        { name : String
        , password : String
        }


type Message
    = Chat ChatMessage
    | System SystemMessage


type alias ChatMessage =
    { sender : String
    , htmlBody : String
    , textBody : String
    , time : Date
    , ownMessage : Bool
    , isPrivate : Bool
    }


type alias SystemMessage =
    { htmlBody : String
    , time : Date
    }


type Msg
    = SubmitLogin
    | SubmitLoginResponse (Result Http.Error UserId)
    | UserInfoResponse (Result Http.Error User)
    | Logout
    | SubmitLogoutResponse (Result Http.Error ())
    | InputLoginName String
    | InputLoginPassword String
    | InputMessage String
    | SendMessage
    | SendMessageResponse (Result Http.Error ())
    | MessagesReceived Currentness (Result String (List Api.Chat.ReceivedMessage))
    | DismissError
    | KeyDown Kbd.KeyCode
    | KeyUp Kbd.KeyCode
    | MessageInputFocus Bool
    | MessageInputMouseOver Bool
    | UpdateTime Time


type Currentness
    = Live
    | Cached


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        cmd =
            Api.Chat.getMessages flags.baseUri Nothing Nothing
                |> Http.send (Result.mapError toString >> MessagesReceived Cached)
    in
        { flags = flags
        , error = Nothing
        , login = Login { name = "", password = "" }
        , messageInputFocused = False
        , messageInputMouseOver = False
        , messageInput = ""
        , messages = Dict.empty
        , ctrlKeyPressed = False
        , currentTime = 0
        }
            ! [ cmd ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        kbdSub =
            Sub.batch [ Kbd.downs KeyDown, Kbd.ups KeyUp ]

        clockSub =
            Time.every Time.second UpdateTime

        wsSub =
            Api.Chat.webSocketSubscription (Result.map List.singleton >> MessagesReceived Live) model.flags.wsUri
    in
        case model.login of
            LoggedIn user ->
                Sub.batch [ wsSub (Just user.id), kbdSub, clockSub ]

            _ ->
                Sub.batch [ wsSub Nothing, kbdSub, clockSub ]


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
            let
                cmd =
                    case model.login of
                        Login _ ->
                            Cmd.none

                        LoggedIn user ->
                            Http.send SubmitLogoutResponse (Api.Chat.logoutRequest model.flags.baseUri user.id)
            in
                { model | login = Login { name = "", password = "" } } ! [ cmd ]

        SubmitLogoutResponse (Ok ()) ->
            model ! []

        SubmitLogoutResponse (Err err) ->
            { model | error = Just (toString err) } ! []

        SubmitLogin ->
            let
                cmd =
                    case model.login of
                        Login loginModel ->
                            Http.send SubmitLoginResponse (Api.Chat.loginRequest model.flags.baseUri loginModel)

                        LoggedIn _ ->
                            Cmd.none
            in
                { model
                    | error = Nothing
                    , messages = Dict.empty
                }
                    ! [ cmd ]

        SubmitLoginResponse (Ok userId) ->
            let
                cmd =
                    Http.send UserInfoResponse (Api.Chat.userInfoRequest model.flags.baseUri userId)
            in
                model ! [ cmd ]

        SubmitLoginResponse (Err err) ->
            case model.login of
                Login loginModel ->
                    { model
                        | login = Login { loginModel | password = "" }
                        , error = Just (toString err)
                    }
                        ! []

                LoggedIn _ ->
                    model ! []

        UserInfoResponse (Ok user) ->
            let
                cmd =
                    Api.Chat.getMessages model.flags.baseUri (Just user.id) Nothing
                        |> Http.send (Result.mapError toString >> MessagesReceived Cached)

                cmdNotify =
                    N.showNotification (N.Notification ("Hi " ++ user.name) "")
            in
                { model | login = LoggedIn user }
                    ! [ cmd, cmdNotify ]

        UserInfoResponse (Err err) ->
            { model | error = Just (toString err) }
                ! []

        InputMessage inp ->
            { model | messageInput = inp }
                ! []

        --- message sending/receiving
        SendMessage ->
            sendMessage model

        SendMessageResponse (Ok ()) ->
            model ! []

        SendMessageResponse (Err err) ->
            { model | error = Just (toString err) }
                ! []

        MessagesReceived currentness (Ok msgs) ->
            let
                mapMsg msg =
                    case msg.data of
                        Api.Chat.Post post ->
                            ( msg.messageNo, Chat <| ChatMessage post.sender post.htmlBody post.textBody msg.time (Just post.sender == currentUserName model) post.isPrivate )

                        Api.Chat.System log ->
                            ( msg.messageNo, System <| SystemMessage log.htmlBody msg.time )

                mapNotify msg =
                    case msg.data of
                        Api.Chat.Post post ->
                            if Just post.sender /= currentUserName model then
                                Just (N.Notification ("Message from " ++ post.sender) post.textBody)
                            else
                                Nothing

                        Api.Chat.System log ->
                            Nothing

                newMsgs =
                    Dict.union (Dict.fromList <| List.map mapMsg msgs) model.messages

                cmdNotifications =
                    case currentness of
                        Live ->
                            List.filterMap mapNotify msgs
                                |> List.map N.showNotification
                                |> Cmd.batch

                        Cached ->
                            Cmd.none
            in
                { model | messages = newMsgs }
                    ! [ cmdNotifications ]

        MessagesReceived _ (Err err) ->
            { model | error = Just err }
                ! []

        MessageInputFocus focused ->
            { model | messageInputFocused = focused } ! []

        MessageInputMouseOver over ->
            { model | messageInputMouseOver = over } ! []

        KeyDown keyCode ->
            if keyCode == ctrlKeyCode then
                { model | ctrlKeyPressed = True } ! []
            else if keyCode == enterKeyCode && model.ctrlKeyPressed && model.messageInputFocused then
                sendMessage model
            else
                model ! []

        KeyUp keyCode ->
            if keyCode == ctrlKeyCode then
                { model | ctrlKeyPressed = False } ! []
            else
                model ! []

        UpdateTime time ->
            { model | currentTime = time } ! []


sendMessage : Model -> ( Model, Cmd Msg )
sendMessage model =
    if model.messageInput == "" then
        model ! []
    else
        let
            cmd =
                case model.login of
                    LoggedIn user ->
                        Http.send SendMessageResponse (Api.Chat.postMessage model.flags.baseUri user.id model.messageInput)

                    Login _ ->
                        Cmd.none
        in
            { model | messageInput = "" } ! [ cmd ]


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
    let
        -- used to make space for the fixed-bottom navbar (all elements should be seen when scrollable)
        -- ignores expanded input though
        bottomSpace =
            H.div [ Attr.style [ ( "height", "75px" ) ] ] []
    in
        H.div
            []
            (viewInput model :: viewMessages model.currentTime model.messages ++ [ bottomSpace ])


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
                        [ H.textarea
                            [ Attr.class "form-control mb-2"
                            , Attr.rows
                                (if model.messageInputFocused || model.messageInputMouseOver then
                                    5
                                 else
                                    1
                                )
                            , Attr.placeholder "message"
                            , Attr.value model.messageInput
                            , Attr.disabled isDisabled
                            , Ev.onInput InputMessage
                            , Ev.onMouseOver (MessageInputMouseOver True)
                            , Ev.onMouseOut (MessageInputMouseOver False)
                            , Ev.onFocus (MessageInputFocus True)
                            , Ev.onBlur (MessageInputFocus False)
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
                                , ( "btn-outline-warning", not isDisabled && model.messageInput == "" )
                                , ( "btn-outline-danger", isDisabled )
                                ]
                            , Attr.disabled (isDisabled || model.messageInput == "")
                            ]
                            [ H.text "send" ]
                        ]
                    ]
                ]
            ]


viewMessages : Time -> Dict MessageId Message -> List (Html Msg)
viewMessages now =
    Dict.values
        >> List.reverse
        >> List.map (viewMessage now)


viewMessage : Time -> Message -> Html Msg
viewMessage time msg =
    case msg of
        Chat chatMsg ->
            viewChatMessage time chatMsg

        System sysMsg ->
            viewSystemMessage time sysMsg


viewChatMessage : Time -> ChatMessage -> Html Msg
viewChatMessage now msg =
    H.div
        [ Attr.class "card w-75 mt-2 mb-2"
        , Attr.classList
            [ ( "float-right", not msg.ownMessage )
            , ( "text-white", msg.isPrivate || msg.ownMessage )
            , ( "bg-info", msg.ownMessage )
            , ( "bg-warning", msg.isPrivate )
            ]
        ]
        [ H.div
            [ Attr.class "card-header" ]
            [ H.div
                [ Attr.class "row" ]
                [ H.div
                    [ Attr.class "col-md-8" ]
                    [ H.h5
                        [ Attr.class "card-title" ]
                        [ H.text msg.sender ]
                    ]
                , H.div
                    [ Attr.class "col" ]
                    [ H.div
                        [ Attr.class "float-right" ]
                        [ H.h6
                            []
                            [ H.text (formatEllapsedTime now msg.time)
                            ]
                        ]
                    ]
                ]
            ]
        , H.div
            [ Attr.class "card-body" ]
            [ rawHtml msg.htmlBody
            ]
        ]


viewSystemMessage : Time -> SystemMessage -> Html Msg
viewSystemMessage now msg =
    H.div
        [ Attr.class "card w-50 mt-2 mb-2 p-0" ]
        [ H.div
            [ Attr.class "card-body" ]
            [ H.div
                [ Attr.class "row" ]
                [ H.div
                    [ Attr.class "col-md-8" ]
                    [ rawHtml msg.htmlBody ]
                , H.div
                    [ Attr.class "col" ]
                    [ H.div
                        [ Attr.class "float-right" ]
                        [ H.h6
                            []
                            [ H.text (formatEllapsedTime now msg.time)
                            ]
                        ]
                    ]
                ]
            ]
        ]


formatEllapsedTime : Time -> Date -> String
formatEllapsedTime currentTime displayDate =
    let
        timeDiff =
            currentTime - Date.toTime displayDate

        ellapsedHours =
            Time.inHours timeDiff

        ellapsedMinutes =
            Time.inMinutes timeDiff

        ellapsedSeconds =
            Time.inSeconds timeDiff
    in
        if round ellapsedHours >= 2 then
            toString (round ellapsedHours) ++ " hours ago"
        else if ellapsedHours >= 1 then
            "one hour ago"
        else if round ellapsedMinutes >= 2 then
            toString (round ellapsedMinutes) ++ " minutes ago"
        else if ellapsedMinutes >= 1 then
            "one minute ago"
        else
            "just now"


rawHtml : String -> Html msg
rawHtml =
    let
        def =
            MD.defaultOptions

        options =
            { def
                | sanitize = False
            }
    in
        MD.toHtmlWith options []
