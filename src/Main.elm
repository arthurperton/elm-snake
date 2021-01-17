module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, button, div, node, text)
import Html.Attributes exposing (class, href, rel, style)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, field, map, string)
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type Msg
    = KeyPressed String
    | Tick Time.Posix


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
    { food : Pos
    , size : Int
    , snakeDirection : Direction
    , snakeHead : Pos
    , snakeTail : List Pos
    , tick : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { food = Pos 3 8
      , size = 20
      , snakeDirection = Right
      , snakeHead = Pos 5 5
      , snakeTail = []
      , tick = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 400 Tick
        , onKeyDown keyDecoder
        ]


keyDecoder : Decoder Msg
keyDecoder =
    map KeyPressed (field "key" string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed key ->
            case directionForKey key of
                Just direction ->
                    ( { model | snakeDirection = direction }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Tick _ ->
            ( { model
                | tick = model.tick + 1
                , snakeHead = move model.snakeDirection model.snakeHead
              }
            , Cmd.none
            )


directionForKey : String -> Maybe Direction
directionForKey key =
    case key of
        "a" ->
            Just Left

        "d" ->
            Just Right

        "w" ->
            Just Up

        "s" ->
            Just Down

        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        _ ->
            Nothing


move : Direction -> Pos -> Pos
move dir pos =
    case dir of
        Left ->
            Pos (pos.x - 1) pos.y

        Right ->
            Pos (pos.x + 1) pos.y

        Up ->
            Pos pos.x (pos.y - 1)

        Down ->
            Pos pos.x (pos.y + 1)


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ node "link" [ rel "stylesheet", href "style.css" ] []
        , div [ class "field" ]
            [ snakeHead model.size model.snakeHead
            ]
        ]


stylePercent : Int -> Int -> String
stylePercent size value =
    String.fromFloat (toFloat value * 100 / toFloat size) ++ "%"


snakeHead : Int -> Pos -> Html Msg
snakeHead size pos =
    div
        [ class "snake snake-head"
        , style "left" (stylePercent size pos.x)
        , style "top" (stylePercent size pos.y)
        , style "width" (stylePercent size 1)
        , style "height" (stylePercent size 1)
        ]
        []
