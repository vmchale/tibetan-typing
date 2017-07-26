port module Main exposing (..)

import Html exposing (..)
import State.Types exposing (..)
import State exposing (showDifficulty, init)
import View exposing (view)
import Update exposing (update)
import Subscriptions exposing (subscriptions)


main =
    programWithFlags { init = init, view = view, update = updateAndStore, subscriptions = subscriptions }


port setStorage : String -> Cmd msg


updateAndStore : Msg -> Model -> ( Model, Cmd Msg )
updateAndStore msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage (showDifficulty newModel.maxDifficulty), cmds ]
        )
