port module Main exposing (..)

import Html exposing (..)
import State.Types exposing (..)
import State exposing (showDifficulty, init)
import View exposing (view)
import Update exposing (update)
import Subscriptions exposing (subscriptions)
import Maybe exposing (withDefault)
import Keyboard exposing (KeyCode)
import List exposing (member, range)


port setStorage : String -> Cmd msg


main : Program (Maybe String) Model Msg
main =
    programWithFlags { init = init, view = view, update = updateAndStore << withDefault None << filterSubscriptions, subscriptions = subscriptions }


{-| Updates are in English, though we should probably change that.
-}
updateAndStore : Msg -> Model -> ( Model, Cmd Msg )
updateAndStore msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage (showDifficulty English newModel.maxDifficulty), cmds ]
        )


filterSubscriptions : Msg -> Maybe Msg
filterSubscriptions x =
    case x of
        KeyMsg k ->
            if controlKeys k then
                (Just (KeyMsg k))
            else
                Nothing

        x ->
            Just x


controlKeys : KeyCode -> Bool
controlKeys x =
    not (member x ([ 9, 13, 16, 17, 18, 20, 27, 45, 46, 91, 116, 144, 112 ] ++ (range 33 40) ++ (range 114 123)))
