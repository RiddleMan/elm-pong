module Util exposing (px, clock)

import Time exposing (..)

px : Float -> String
px unit =
    toString unit ++ "px"

clock : (Time -> x) -> Sub x
clock x =
    every (1000 / 60) x
