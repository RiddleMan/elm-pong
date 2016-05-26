import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Time exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
    { position : Position
    , drag : Maybe Drag
    , timer: Int
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
  ( Model (Position 200 200) Nothing 1, Cmd.none )



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({position, drag, timer} as model) =
  case msg of
    DragStart xy ->
      Model position (Just (Drag xy xy)) timer

    DragAt xy ->
      Model position (Maybe.map (\{start} -> Drag start xy) drag) timer

    DragEnd _ ->
      Model (getPosition model) Nothing timer

    Tick _ ->
      Model position drag (timer ** (timer + 1))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      every second Tick

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd, every second Tick ]
-- VIEW
square : number -> number -> number
square a b =
  a^b

(**) = (square)
infixr 3 **
  
(=>) = (,)

view : Model -> Html Msg
view model =
  let
    realPosition =
      getPosition model
  in
    div
      [ onMouseDown
      , style
          [ "background-color" => "#3C8D2F"
          , "cursor" => "move"

          , "width" => "100px"
          , "height" => "100px"
          , "border-radius" => "4px"
          , "position" => "absolute"
          , "left" => px realPosition.x
          , "top" => px realPosition.y

          , "color" => "white"
          , "display" => "flex"
          , "align-items" => "center"
          , "justify-content" => "center"
          ]
      ]
      [ text (toString (model.timer))
      ]


px : Int -> String
px number =
  toString number ++ "px"


getPosition : Model -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)


onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)
