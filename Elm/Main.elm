module Main exposing (..)

import Api.Chat exposing (UserId, User, ReceivedMessage)
import Date exposing (Date)
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Keyboard as Kbd
import Markdown as MD
import Time exposing (Time)


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
    , messages : List ChatMessage
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


type alias ChatMessage =
    { sender : String
    , htmlBody : String
    , time : Date
    , ownMessage : Bool
    , isPrivate : Bool
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
    | MessagesReceived (Result String (List Api.Chat.ReceivedMessage))
    | DismissError
    | KeyDown Kbd.KeyCode
    | KeyUp Kbd.KeyCode
    | MessageInputFocus Bool
    | MessageInputMouseOver Bool
    | UpdateTime Time


init : Flags -> ( Model, Cmd Msg )
init flags =
    { flags = flags
    , error = Nothing
    , login = Login { name = "", password = "" }
    , messageInputFocused = False
    , messageInputMouseOver = False
    , messageInput = ""
    , messages = []
    , ctrlKeyPressed = False
    , currentTime = 0
    }
        ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        kbdSub =
            Sub.batch [ Kbd.downs KeyDown, Kbd.ups KeyUp ]

        clockSub =
            Time.every Time.second UpdateTime
    in
        case model.login of
            LoggedIn user ->
                let
                    wsSub =
                        user.id
                            |> Api.Chat.webSocketSubscription (Result.map List.singleton >> MessagesReceived) model.flags.wsUri
                in
                    Sub.batch [ wsSub, kbdSub, clockSub ]

            _ ->
                Sub.batch [ kbdSub, clockSub ]


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
                            Http.send SubmitLoginResponse (Api.Chat.loginRequest model.flags.baseUri loginModel)

                        LoggedIn _ ->
                            Cmd.none
            in
                { model | error = Nothing } ! [ cmd ]

        SubmitLoginResponse (Ok userId) ->
            let
                cmd =
                    Http.send UserInfoResponse (Api.Chat.userInfoRequest model.flags.baseUri userId)
            in
                model ! [ cmd ]

        SubmitLoginResponse (Err err) ->
            case model.login of
                Login loginModel ->
                    { model | login = Login { loginModel | password = "" } } ! []

                LoggedIn _ ->
                    model ! []

        UserInfoResponse (Ok user) ->
            { model | login = LoggedIn user }
                ! [ Api.Chat.getMessages model.flags.baseUri user.id Nothing
                        |> Http.send (Result.mapError toString >> MessagesReceived)
                  ]

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

        MessagesReceived (Ok msgs) ->
            let
                mapMsg msg =
                    case msg.data of
                        Api.Chat.Post post ->
                            ChatMessage post.sender post.htmlBody msg.time (Just post.sender == currentUserName model) post.isPrivate

                newMsgs =
                    List.map mapMsg msgs ++ model.messages
            in
                { model | messages = newMsgs }
                    ! []

        MessagesReceived (Err err) ->
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


viewMessages : Time -> List ChatMessage -> List (Html Msg)
viewMessages now =
    List.map (viewMessage now)


viewMessage : Time -> ChatMessage -> Html Msg
viewMessage now msg =
    H.div
        [ Attr.class "card w-75 mb-2"
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
