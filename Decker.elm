module Decker exposing (view, update, Model, init, subscriptions, Msg)

import Html exposing (..)
import Html.Attributes exposing (..)
import Window
import Task
import Util exposing (..)
import Keyboard exposing (..)
import Char
import Time exposing (Time)
import Debug exposing (..)

type Direction =
    Up
    | Down

type Msg =
    Run
    | Stop
    | MoveStart KeyCode
    | MoveEnd KeyCode
    | Tick Time
    | Screen Int

type alias Move =
    Maybe Direction

type alias Model =
    { position: Float
    , initialized: Bool
    , height: Int
    , screen: Int
    , velocity: Float
    , upKey: KeyCode
    , downKey: KeyCode
    , moveable: Bool
    , moving: Move
    , direction: Bool
    }

type alias Controls =
    { up: Char
    , down: Char
    }

getScreenDim : Cmd Msg
getScreenDim =
    Task.perform Screen Screen Window.height

init : Controls -> Bool -> (Model, Cmd Msg)
init { up, down } d =
    ({ position = 0
    , height = 70
    , screen = 0
    , velocity = 5
    , initialized = False
    , moveable = True
    , upKey = Char.toCode up
    , downKey = Char.toCode down
    , moving = Nothing
    , direction = d
    }, getScreenDim)

moveDeck : Float -> Direction -> Int -> Float -> Float
moveDeck position direction screen velocity =
    let
        pos = case direction of
            Up ->
                position - velocity
            Down ->
                position + velocity
    in
        if pos < 0
            then 0
            else if pos > screen
                then toFloat screen
                else pos

updateMove : Model -> Model
updateMove model =
    let
        { position, moving, screen, velocity } = model
    in
        case moving of
            Nothing ->
                model
            Just d ->
                { model | position = (moveDeck position d screen velocity) }

keyCodeToMoving : KeyCode -> Model -> Move
keyCodeToMoving keyCode { upKey, downKey } =
    if keyCode == upKey
        then Just Up
        else if keyCode == downKey
            then Just Down
            else Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Run ->
            ({ model | moveable = True }, Cmd.none)
        Stop ->
            ({ model | moveable = False }, Cmd.none)

        MoveStart code ->
            ({ model | moving = keyCodeToMoving code model }, Cmd.none)
        MoveEnd code ->
            ({ model | moving = Nothing }, Cmd.none)

        Tick _ ->
            (updateMove model, Cmd.none)
        Screen height ->
            ({ model | screen = height, initialized = True }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions { moveable, initialized, upKey, downKey} =
    if moveable && initialized
        then Sub.batch [
            clock Tick,
            downs MoveStart,
            ups MoveEnd
        ]
        else Sub.none

view : Model -> Html Msg
view { position, height, direction } =
    div [
        style [
            ("background", "#000"),
            ("width", px 10),
            ("height", px (toFloat height)),
            ("position", "absolute"),
            ("top", px 40),
            case direction of
                True ->
                    ("left", "0")
                False ->
                    ("right", "0")
        ]
    ] []
