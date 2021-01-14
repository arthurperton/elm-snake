module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type Msg
    = Tick Time.Posix


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Pos =
    { x : Int
    , y : Int
    }


type Object
    = None
    | Snake
    | Apple



-- type alias Object =
--     { type_ : ObjectType
--     , position : Pos
--     }
-- type alias Row =
--     Array Object


type alias Grid =
    Array (Array Object)


type alias Model =
    { direction : Direction
    , grid : Grid
    , length : Int
    , position : Pos
    , tick : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { direction = Right
      , grid = Array.repeat 11 (Array.repeat 11 None) -- TODO configurable size
      --   , objects = [ Object Snake (Pos 5 5) ]
      , length = 1
      , position = Pos 5 5
      , tick = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | tick = model.tick + 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ style "background-color" "#fef" ]
        (Array.toList (Array.map (\row -> div [] [ text "row"]) model.grid))
