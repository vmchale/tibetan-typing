module Subscriptions exposing (subscriptions)

import Platform.Sub exposing (batch)
import Keyboard exposing (KeyCode, downs)
import State exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    batch
        [ downs KeyMsg
        ]
