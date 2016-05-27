module Ball exposing (view, update, Model, init, subscriptions, Msg)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Window
import Task
import Util exposing (..)

type alias Position =
    (Float, Float)

type alias Size =
    { w: Int
    , h: Int
    }

type alias Vector =
    (Float, Float)

type alias Model =
    { initialized: Bool
    , position: Position
    , moveable: Bool
    , bounce: Int
    , move: Vector
    , screen: Window.Size
    , ball: Size
    }

type Msg = Run
    | FakeFail
    | Stop
    | Tick Time
    | Bounce
    | Screen Window.Size

getScreenDim : Cmd Msg
getScreenDim =
    Task.perform Screen Screen Window.size

subscriptions : Model -> Sub Msg
subscriptions {moveable, initialized} =
    if moveable && initialized then
        clock Tick
    else
        Sub.none


init : (Model, Cmd Msg)
init =
    ({ position = (50, 50)
    , initialized = False
    , moveable = True
    , bounce = 5
    , move = (0.75, -0.75)
    , screen = Window.Size 0 0
    , ball = {w = 20, h = 20}
    }, getScreenDim)

getVelocity : Int -> Float
getVelocity bounces =
    toFloat bounces * 0.5

moveBallCoord : Float -> Float -> Float -> Float -> Int -> Float
moveBallCoord coord mCoord v end ball =
    let
        pos = coord + (mCoord * v)
    in
        if pos <= 0
            then 0
            else if pos >= end - (toFloat ball)
                then end - (toFloat ball)
                else pos

moveBall : Position -> Vector -> Float -> Window.Size -> Size -> Position
moveBall (x, y) (mX, mY) v screen ball =
    (moveBallCoord x mX v (toFloat screen.width) ball.w, moveBallCoord y mY v (toFloat screen.height) ball.h)

bounce : Model -> Model
bounce model =
    let
        { bounce, move } = model
        (dX, dY) = move
    in
        { model |
            bounce = bounce + 1,
            move = (-dX, dY) }

isBounced : Position -> Window.Size -> Size -> Bool
isBounced (x, y) size ball =
    x == 0 || y == 0 || x == size.width - ball.w || y == size.height - ball.h

getMoveVector : Vector -> Position -> Window.Size -> Size -> Vector
getMoveVector (dX, dY) (x, y) screen ball =
    (if x == 0 || x == (screen.width - ball.w) then -dX else dX
    , if y == 0 || y == (screen.height - ball.h) then -dY else dY)

updatePos : Model -> Model
updatePos model =
    let
        {position, bounce, move, screen, ball} = model

        velocity = getVelocity bounce

        newPosition = moveBall position move velocity screen ball
        moveVector = getMoveVector move newPosition screen ball
        newBounce = if isBounced newPosition screen ball
            then bounce + 1
            else bounce
    in
        {model |
            position = newPosition,
            move = moveVector,
            bounce = newBounce}

updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Run ->
            { model | moveable = True }
        Stop ->
            { model | moveable = False }
        Tick _ ->
            updatePos model
        Bounce ->
            bounce model
        FakeFail ->
            model
        Screen size ->
            { model | screen = size, initialized = True}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (updateModel msg model, Cmd.none)

view : Model -> Html Msg
view {position, ball} =
    let
        (x, y) = position
        {w, h} = ball
    in
        div [
            style [
                ("background", "#233444"),
                ("will-change", "top, left"),
                ("position", "absolute"),
                ("border-radius", "10px"),
                ("width", px (toFloat w)),
                ("height", px (toFloat h)),
                ("top", px y),
                ("left", px x)
            ]
        ] []
