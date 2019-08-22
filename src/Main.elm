module Main exposing (Model, Msg(..), decodeLatLong, init, leafletMap, main, onFloatInput, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (..)



-- main : Program Never Model Msg


main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { latitude : Float
    , longitude : Float
    , zoom : String
    }


init : ( Model, Cmd msg )
init =
    ( { latitude = 48.2082
      , longitude = 16.3738
      , zoom = "5"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetLatitude Float
    | SetLongitude Float
    | SetLatLong Float Float
    | SetZoom String


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        SetLatitude latitude ->
            ( { model | latitude = latitude }, Cmd.none )

        SetLongitude longitude ->
            ( { model | longitude = longitude }, Cmd.none )

        SetLatLong latitude longitude ->
            ( { model | latitude = latitude, longitude = longitude }, Cmd.none )

        SetZoom zoom ->
            ( { model | zoom = zoom }, Cmd.none )


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
                , Html.Attributes.value (String.fromFloat (model.latitude * 10000))
                , onFloatInput SetLatitude
                ]
                []
            , span [] [ text (String.fromFloat model.latitude) ]
            ]
        , div []
            [ label [] [ text "Longitude" ]
            , input
                [ type_ "range"
                , Html.Attributes.min "-1800000"
                , Html.Attributes.max "1800000"
                , Html.Attributes.value (String.fromFloat (model.longitude * 10000))
                , onFloatInput SetLongitude
                ]
                []
            , span [] [ text (String.fromFloat model.longitude) ]
            ]
        , div []
            [ label [] [ text "Zoom" ]
            , input
                [ type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "18"
                , Html.Attributes.value model.zoom
                , onInput SetZoom
                ]
                []
            , span [] [ text model.zoom ]
            ]
        , img
            [ class "elm-logo"
            , src "http://package.elm-lang.org/assets/elm_logo.svg"
            ]
            []
        , leafletMap
            [ attribute "latitude" (String.fromFloat model.latitude)
            , attribute "longitude" (String.fromFloat model.longitude)
            , attribute "zoom" model.zoom
            , on "moveend"
                (map2 SetLatLong
                    (at [ "target", "latitude" ] float)
                    (at [ "target", "longitude" ] float)
                )
            , on "zoomend"
                (at [ "target", "zoom" ] int
                    |> Decode.map (String.fromInt >> SetZoom)
                )
            ]
            []
        ]


onFloatInput : (Float -> Msg) -> Attribute Msg
onFloatInput toMsg =
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
            Decode.fail "err"
