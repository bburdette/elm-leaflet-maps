module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { latitude : Float
    , longitude : Float
    }


init : ( Model, Cmd msg )
init =
    ( { latitude = 48.2082, longitude = 16.3738 }, Cmd.none )



-- UPDATE


type Msg
    = SetLatitude Float
    | SetLongitude Float
    | SetLatLong Float Float


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        SetLatitude latitude ->
            { model | latitude = latitude } ! []

        SetLongitude longitude ->
            { model | longitude = longitude } ! []

        SetLatLong latitude longitude ->
            { model | latitude = latitude, longitude = longitude } ! []


leafletMap : List (Attribute a) -> List (Html a) -> Html a
leafletMap =
    Html.node "leaflet-map"


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [] [ text "Latitude" ]
            , input
                [ type_ "range"
                , Html.Attributes.min "-1800000"
                , Html.Attributes.max "1800000"
                , defaultValue (toString model.latitude)
                , onInput SetLatitude
                ]
                []
            , span [] [ text (toString model.latitude) ]
            ]
        , div []
            [ label [] [ text "Longitude" ]
            , input
                [ type_ "range"
                , Html.Attributes.min "-1800000"
                , Html.Attributes.max "1800000"
                , defaultValue (toString model.longitude)
                , onInput SetLongitude
                ]
                []
            , span [] [ text (toString model.longitude) ]
            ]
        , img
            [ class "elm-logo"
            , src "http://package.elm-lang.org/assets/elm_logo.svg"
            ]
            []
        , leafletMap
            [ attribute "latitude" (toString model.latitude)
            , attribute "longitude" (toString model.longitude)
            , attribute "zoom" "5"
            , on "move"
                (map2 SetLatLong
                    (at [ "target", "latitude" ] float)
                    (at [ "target", "longitude" ] float)
                )
            ]
            []
        ]


onInput : (Float -> Msg) -> Attribute Msg
onInput toMsg =
    Decode.string
        |> Decode.andThen decodeLatLong
        |> Decode.at [ "target", "value" ]
        |> Decode.map toMsg
        |> on "input"


decodeLatLong : String -> Decoder Float
decodeLatLong str =
    case Decode.decodeString Decode.float str of
        Ok num ->
            Decode.succeed (num / 10000)

        Err err ->
            Decode.fail err
