module Ball exposing (view, update, Model, init, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Window
import Task

type alias Position =
    (Float, Float)

type alias Vector =
    (Float, Float)

type alias Model =
    { initialized: Bool
    , position: Position
    , moveable: Bool
    , bounce: Int
    , move: Vector
    , screen: Window.Size
    }

type Msg = Run
    | FakeFail
    | Stop
    | Tick Time
    | Bounce
    | Screen Window.Size

getScreenDim : Cmd Msg
getScreenDim =
    Task.perform (\x -> FakeFail) Screen Window.size

subscriptions : Model -> Sub Msg
subscriptions {moveable, initialized} =
    if moveable && initialized then
        every (1000 / 60) Tick
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
    }, getScreenDim)

px : Float -> String
px unit =
    toString unit ++ "px"

getVelocity : Int -> Float
getVelocity bounces =
    toFloat bounces * 0.5

moveBallCoord : Float -> Float -> Float -> Float -> Float
moveBallCoord coord mCoord v end =
    let
        pos = coord + (mCoord * v)
    in
        if pos <= 0
            then 0
            else if pos >= end
                then end
                else pos

moveBall : Position -> Vector -> Float -> Window.Size -> Position
moveBall (x, y) (mX, mY) v screen =
    (moveBallCoord x mX v (toFloat screen.width), moveBallCoord y mY v (toFloat screen.height))

bounce : Model -> Model
bounce model =
    let
        { bounce, move } = model
        (dX, dY) = move
    in
        { model |
            bounce = bounce + 1,
            move = (-dX, dY) }

isBounced : Position -> Window.Size -> Bool
isBounced (x, y) size =
    x == 0 || y == 0 || x == size.width || y == size.height

getMoveVector : Vector -> Position -> Window.Size -> Vector
getMoveVector (dX, dY) (x, y) screen =
    (if x == 0 || x == screen.width then -dX else dX
    , if y == 0 || y == screen.height then -dY else dY)

updatePos : Model -> Model
updatePos model =
    let
        {position, bounce, move, screen} = model

        velocity = getVelocity bounce

        newPosition = moveBall position move velocity screen
        moveVector = getMoveVector move newPosition screen
        newBounce = if isBounced newPosition screen
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
view {position} =
    let
        (x, y) = position
    in
        div [
            style [
                ("background", "#233444"),
                ("will-change", "top, left"),
                ("position", "absolute"),
                ("border-radius", "10px"),
                ("width", "20px"),
                ("height", "20px"),
                ("top", px y),
                ("left", px x)
            ]
        ] []
