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
    , size : Int
    , snakeDirection : Direction
    , snakeIsDead : Bool
    , snakeHead : Pos
    , snakeTail : List Pos
    , snakeTailLength : Int
    , tick : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        size = 12
    in
    ( { food = Pos 3 8
      , size = size
      , snakeDirection = Right
      , snakeIsDead = False
      , snakeHead = Pos 5 5
      , snakeTail = [ Pos 4 5, Pos 3 5 ]
      , snakeTailLength = 2
      , tick = 0
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
        KeyPressed "r" ->
            init ()

        KeyPressed key ->
            case directionForKey key of
                Just direction ->
                    ( { model | snakeDirection = direction }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        MoveFoodTo ( x, y ) ->
            ( { model | food = Pos x y }, Cmd.none )

        Tick _ ->
            tick model


tick : Model -> ( Model, Cmd Msg )
tick model =
    let
        snakeHead =
            case model.snakeIsDead of
                False ->
                    move model.snakeHead model.snakeDirection

                True ->
                    model.snakeHead

        snakeHasFood =
            snakeHead == model.food

        snakeIsDead =
            model.snakeIsDead
                || (snakeHead.x < 0)
                || (snakeHead.y < 0)
                || (snakeHead.x >= model.size)
                || (snakeHead.y >= model.size)
                || List.member snakeHead model.snakeTail

        snakeTailLength =
            case snakeHasFood of
                False ->
                    model.snakeTailLength

                True ->
                    model.snakeTailLength + 1

        snakeTail =
            case model.snakeIsDead of
                False ->
                    updateTail model.snakeHead model.snakeTail snakeTailLength

                True ->
                    model.snakeTail

        cmd =
            case snakeHasFood of
                False ->
                    Cmd.none

                True ->
                    moveFood model.size
    in
    ( { model
        | tick = model.tick + 1
        , snakeHead = snakeHead
        , snakeIsDead = snakeIsDead
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



-- newNumber : Cmd Msg
-- newNumber =
--     Random.generate NewNumber oneToTen


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
        , div [ class "score" ] [ text (String.fromInt (model.snakeTailLength - 2))]
        , div [ class "field" ]
            (viewFood model.size model.food
                :: viewSnakeBlood model.size model.snakeHead model.snakeIsDead
                :: viewSnakeHead model.size model.snakeHead
                :: viewSnakeTail model.size model.snakeTail
            )
        ]


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


viewObject : Int -> Pos -> String -> Html Msg
viewObject size pos class_ =
    div
        [ class class_
        , style "left" (stylePercent size pos.x)
        , style "top" (stylePercent size pos.y)
        , style "width" (stylePercent size 1)
        , style "height" (stylePercent size 1)
        ]
        []
