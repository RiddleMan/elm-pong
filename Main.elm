import Html.App as Html
import Html exposing (..)
import Ball
import Decker

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { deck1: Decker.Model
    , deck2: Decker.Model
    , ball: Ball.Model
    }

type Msg =
    Deck1 Decker.Msg
    | Deck2 Decker.Msg
    | Ball Ball.Msg

init : (Model, Cmd Msg)
init =
    let
        (deck1M, deck1C) = Decker.init {
            up = 'w', down = 's'
        } True
        (deck2M, deck2C) = Decker.init {
            up = 'i', down = 'k'
        } False
        (ballM, ballC) = Ball.init

        cmds = Cmd.batch [
            Cmd.map Deck1 deck1C,
            Cmd.map Deck2 deck2C,
            Cmd.map Ball ballC
        ]
    in
        ({ deck1 = deck1M
        , deck2 = deck2M
        , ball = ballM
        }, cmds)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Deck1 m ->
            let
                (deck, effect) = (Decker.update m model.deck1)
            in
                ({ model | deck1 = deck }, Cmd.map Deck1 effect)
        Deck2 m ->
            let
                (deck, effect) = Decker.update m model.deck2
            in
                ({ model | deck2 = deck }, Cmd.map Deck2 effect)
        Ball m ->
            let
                (ball, effect) = Ball.update m model.ball
            in
                ({ model | ball = ball }, Cmd.map Ball effect)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Sub.map Ball (Ball.subscriptions model.ball),
        Sub.map Deck1 (Decker.subscriptions model.deck1),
        Sub.map Deck2 (Decker.subscriptions model.deck2)
    ]

view : Model -> Html Msg
view { deck1, deck2, ball} =
    div [] [
        Html.map Ball (Ball.view ball),
        Html.map Deck1 (Decker.view deck1),
        Html.map Deck2 (Decker.view deck2)
    ]
