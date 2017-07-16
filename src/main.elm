module Main exposing (..)

import Html exposing (..)
import State exposing (..)
import View exposing (view)
import Update exposing (update)
import Subscriptions exposing (subscriptions)


main =
    program { init = init, view = view, update = update, subscriptions = subscriptions }
