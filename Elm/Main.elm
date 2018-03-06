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
    | InputLoginName String
    | InputLoginPassword String


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
        SubmitLoginResponse (Ok userId) ->
            let
                cmd =
                    Http.send UserInfoResponse (Api.Chat.userInfoRequest baseUrl userId)
            in
                model ! [ cmd ]

        SubmitLoginResponse (Err err) ->
            { model | error = Just (toString err) }
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
        content =
            case model.view of
                Login loginModel ->
                    viewLogin loginModel

                Chat chatModel ->
                    viewChat chatModel
    in
        H.div
            []
            [ viewError model.error
            , viewUser model.user
            , content
            ]


viewUser : Maybe User -> Html Msg
viewUser user =
    case user of
        Nothing ->
            H.text ""

        Just user ->
            H.p [] [ H.text "hello, ", H.strong [] [ H.text user.name ] ]


viewError : Maybe String -> Html Msg
viewError err =
    case err of
        Nothing ->
            H.text ""

        Just text ->
            H.p [] [ H.strong [] [ H.text "error: " ], H.text text ]


viewLogin : LoginModel -> Html Msg
viewLogin model =
    H.div
        []
        [ H.form
            [ Ev.onSubmit SubmitLogin ]
            [ H.div
                []
                [ H.input
                    [ Attr.type_ "text", Attr.placeholder "username", Attr.autocomplete False, Ev.onInput InputLoginName, Attr.value model.name ]
                    []
                ]
            , H.div
                []
                [ H.input
                    [ Attr.type_ "password", Attr.placeholder "password", Ev.onInput InputLoginPassword, Attr.value model.password ]
                    []
                ]
            , H.button [ Attr.type_ "submit" ] [ H.text "login" ]
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
