import Html.App as Html
import Ball

main =
    Html.program
        { init = Ball.init
        , view = Ball.view
        , update = Ball.update
        , subscriptions = Ball.subscriptions
        }
