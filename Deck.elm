module Deck exposing (view, update, Model, init, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Window
import Task
import Util exposing (..)

type Direction = Up
	| Down

type Msg = Run
	| Stop
	| Move Direction
	| Screen Window.Size

view : Model -> Html Msg
view =
div [
style [
("background", "#000"),
("width", px 10),
("height", px 70),
("position", "absolute"),
("top", px 40),
]
]
