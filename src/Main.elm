module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, node, text)
import Html.Attributes exposing (class, href, rel, style)
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


type alias Model =
    { snakeDirection : Direction
    , snakeHead : Pos
    , snakeTail : List Pos
    , food : Pos
    , tick : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { snakeDirection = Right
      , snakeHead = Pos 5 5
      , snakeTail = []
      , food = Pos 3 8
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
            ( { model
                | tick = model.tick + 1
                , snakeHead = Pos (model.snakeHead.x + 1) model.snakeHead.y
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ node "link" [ rel "stylesheet", href "style.css" ] []
        , div [ class "field" ]
            [ snakeHead model.snakeHead

            -- (List.head model.snake)
            ]
        ]


stylePercent : Int -> String
stylePercent coord =
    String.fromInt (coord * 10) ++ "%"


snakeHead : Pos -> Html Msg
snakeHead pos =
    div
        [ class "snake snake-head"
        , style "left" (stylePercent pos.x)
        , style "top" (stylePercent pos.y)
        ]
        []
