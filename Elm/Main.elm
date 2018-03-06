module Main exposing (..)

import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Api.Chat exposing (UserId, User)
import Http


baseUrl : String
baseUrl =
    "http://localhost:8081"


main : Program Never Model Msg
main =
    H.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
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
    {}


type Msg
    = SubmitLogin
    | SubmitLoginResponse (Result Http.Error UserId)
    | UserInfoResponse (Result Http.Error User)
    | Logout
    | InputLoginName String
    | InputLoginPassword String
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


initChat : View
initChat =
    Chat {}


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
                    H.text ""

                Chat chatModel ->
                    viewChat chatModel
    in
        H.div
            []
            [ viewNavbar navbarContent
            , H.div
                [ Attr.class "container" ]
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
        [ Attr.class "navbar navbar-expand-lg navbar-light bg-light justify-content-between" ]
        ([ H.a
            [ Attr.class "navbar-brand", Attr.href "#" ]
            [ H.text "Lambda-Chat" ]
         ]
            ++ userContent
        )


viewLogin : LoginModel -> Html Msg
viewLogin model =
    H.form
        [ Attr.class "form-inline", Ev.onSubmit SubmitLogin ]
        [ H.input
            [ Attr.type_ "text"
            , Attr.class "form-control mr-sm-2"
            , Attr.placeholder "username"
            , Attr.autocomplete False
            , Ev.onInput InputLoginName
            , Attr.value model.name
            ]
            []
        , H.input
            [ Attr.type_ "password"
            , Attr.class "form-control mr-sm-2"
            , Attr.placeholder "password"
            , Ev.onInput InputLoginPassword
            , Attr.value model.password
            ]
            []
        , H.button
            [ Attr.type_ "submit"
            , Attr.class "btn btn-outline-success my-2 my-sm-0"
            ]
            [ H.text "login" ]
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
                    , Attr.class "btn btn-outline-success my-2 my-sm-0"
                    ]
                    [ H.text "logout" ]
                ]
            ]


viewChat : ChatModel -> Html Msg
viewChat model =
    H.div
        []
        [ H.h1 [] [ H.text "CHAT" ]
        ]


setView : (a -> View) -> Model -> ( a, cmd ) -> ( Model, cmd )
setView wrap model ( viewModel, cmd ) =
    ( { model | view = wrap viewModel }, cmd )
