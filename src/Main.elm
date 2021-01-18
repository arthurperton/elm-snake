module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, button, div, node, text)
import Html.Attributes exposing (class, href, rel, style)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, field, map, string)
import Random
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type Msg
    = KeyPressed String
    | MoveFoodTo ( Int, Int )
    | Tick Time.Posix


type GamePhase
    = Ready
    | Playing
    | Dead


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Model =
    { food : Pos
    , phase : GamePhase
    , size : Int
    , snakeDirection : Direction
    , snakeHead : Pos
    , snakeTail : List Pos
    , snakeTailLength : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        size =
            12
    in
    ( { food = Pos 3 8
      , phase = Ready
      , size = size
      , snakeDirection = Right
      , snakeHead = Pos 5 5
      , snakeTail = [ Pos 4 5, Pos 3 5 ]
      , snakeTailLength = 2
      }
    , moveFood size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 Tick
        , onKeyDown keyDecoder
        ]


keyDecoder : Decoder Msg
keyDecoder =
    map KeyPressed (field "key" string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed " " ->
            case model.phase of
                Ready ->
                    ( { model | phase = Playing }, Cmd.none )

                Dead ->
                    init ()

                _ ->
                    ( model, Cmd.none )

        KeyPressed key ->
            if model.phase == Playing then
                case directionForKey key of
                    Just direction ->
                        ( { model | snakeDirection = direction }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        MoveFoodTo ( x, y ) ->
            ( { model | food = Pos x y }, Cmd.none )

        Tick _ ->
            tick model


tick : Model -> ( Model, Cmd Msg )
tick model =
    let
        snakeHead =
            if model.phase == Playing then
                move model.snakeHead model.snakeDirection

            else
                model.snakeHead

        snakeHasFood =
            snakeHead == model.food

        snakeTailLength =
            case snakeHasFood of
                False ->
                    model.snakeTailLength

                True ->
                    model.snakeTailLength + 1

        snakeTail =
            if model.phase == Playing then
                updateTail model.snakeHead model.snakeTail snakeTailLength

            else
                model.snakeTail

        phase =
            if
                model.phase
                    == Playing
                    && ((snakeHead.x < 0)
                            || (snakeHead.y < 0)
                            || (snakeHead.x >= model.size)
                            || (snakeHead.y >= model.size)
                            || List.member snakeHead model.snakeTail
                       )
            then
                Dead

            else
                model.phase

        cmd =
            case snakeHasFood of
                False ->
                    Cmd.none

                True ->
                    moveFood model.size
    in
    ( { model
        | phase = phase
        , snakeHead = snakeHead
        , snakeTail = snakeTail
        , snakeTailLength = snakeTailLength
      }
    , cmd
    )


randomCoords : Int -> Random.Generator ( Int, Int )
randomCoords size =
    Random.pair (Random.int 0 (size - 1)) (Random.int 0 (size - 1))


moveFood : Int -> Cmd Msg
moveFood size =
    Random.generate MoveFoodTo (randomCoords size)


updateTail : Pos -> List Pos -> Int -> List Pos
updateTail head tail tailLength =
    head :: List.take (tailLength - 1) tail


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


move : Pos -> Direction -> Pos
move pos dir =
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
        , div [ class "score" ] [ text (String.fromInt (model.snakeTailLength - 2)) ]
        , div [ class "field" ]
            (viewFood model.size model.food
                :: viewSnakeBlood model.size model.snakeHead (model.phase == Dead)
                :: viewSnakeHead model.size model.snakeHead
                :: viewSnakeTail model.size model.snakeTail
            )
        , div [ class "message" ] [ text (message model.phase) ]
        ]


message : GamePhase -> String
message phase =
    case phase of
        Ready ->
            "Press space to start"

        _ ->
            ""


stylePercent : Int -> Int -> String
stylePercent size value =
    String.fromFloat (toFloat value * 100 / toFloat size) ++ "%"


viewFood : Int -> Pos -> Html Msg
viewFood size pos =
    viewObject size pos "food"


viewSnakeBlood : Int -> Pos -> Bool -> Html Msg
viewSnakeBlood size pos isDead =
    viewObject size
        pos
        ("snake snake-blood"
            ++ (if isDead then
                    " snake-blood-spilled"

                else
                    ""
               )
        )


viewSnakeHead : Int -> Pos -> Html Msg
viewSnakeHead size pos =
    viewObject size pos "snake snake-head"


viewSnakeTail : Int -> List Pos -> List (Html Msg)
viewSnakeTail size tail =
    List.map (\pos -> viewObject size pos "snake snake-tail") tail


viewSnakeEyes : Int -> Pos -> Direction -> Html Msg
viewSnakeEyes size pos direction =
    viewObjectWithChildren size pos "snake snake-eyes" [ text "hi" ]


viewObject : Int -> Pos -> String -> Html Msg
viewObject size pos className =
    viewObjectWithChildren size pos className []


viewObjectWithChildren : Int -> Pos -> String -> List (Html Msg) -> Html Msg
viewObjectWithChildren size pos className children =
    div
        [ class className
        , style "left" (stylePercent size pos.x)
        , style "top" (stylePercent size pos.y)
        , style "width" (stylePercent size 1)
        , style "height" (stylePercent size 1)
        ]
        children
