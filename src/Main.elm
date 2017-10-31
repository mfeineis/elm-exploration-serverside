module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Encode as Encode exposing (Value)


type HttpMethod
    = Get
    | Post


type Route
    = HomeRoute


type Msg
    = RawHttpRequest
        { headers : Dict String String
        , method : HttpMethod
        , query : Dict String String
        , route : Route
        }
    | IO Value


type HomeMsg
    = Init
    | SomeIO Value


type alias HomeRequest =
    { route : Route
    }


type Model
    = Unknown { route : Maybe Route }
    | HomeModel HomeRequest


main : Program Never Model Msg
main =
    Html.program
        { init = init (Encode.object [])
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : Value -> ( Model, Cmd Msg )
init flags =
    ( Unknown { route = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RawHttpRequest { method, route } ->
            case ( method, route ) of
                ( Get, HomeRoute ) ->
                    let
                        ( response, cmd ) =
                            handleHomeRoute Init { route = route }
                    in
                    ( HomeModel response, cmd )

                _ ->
                    ( Unknown { route = Nothing }, Cmd.none )

        IO value ->
            case model of
                Unknown _ ->
                    Debug.crash "No IO possible for unknown routes"
                        ( model, Cmd.none )

                HomeModel request ->
                    let
                        ( response, cmd ) =
                            handleHomeRoute (SomeIO value) request
                    in
                    ( HomeModel response, cmd )


handleHomeRoute : HomeMsg -> HomeRequest -> ( HomeRequest, Cmd Msg )
handleHomeRoute msg request =
    case msg of
        Init ->
            ( request, Cmd.none )
            
        SomeIO value ->
            ( request, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Unknown { route } ->
            Html.text ("Unknown route: " ++ toString route)

        HomeModel { route } ->
            Html.text ("HomeModel: " ++ toString route)
