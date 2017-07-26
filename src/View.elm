module View exposing (view)

import Html exposing (..)
import State.Types exposing (..)
import State exposing (showDifficulty)
import Html.Attributes exposing (style)
import Color exposing (..)
import List exposing (..)
import String exposing (concat, fromChar)
import Tuple exposing (first, second)
import Json.Decode as Json
import Html.Events exposing (on, keyCode)
import Html.Attributes exposing (attribute)
import View.Styles exposing (..)
import View.Content exposing (..)


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


view : Model -> Html Msg
view model =
    div [ style pageStyles ]
        [ div [] [ p [ style [ ( "text-align", "right" ) ] ] [ span [ largeText model.largeText ] [ text "Press <F2> to toggle large text" ] ] ]
        , p []
            [ span [ largeText model.largeText ] [ text "Type the following: " ]
            ]
        , p []
            [ span [ colorAttribute green, tibetanText model.largeText ] [ text model.completed ]
            , span [ colorFailed model.failed, tibetanText model.largeText ] [ text << fromChar <| model.nextChar ]
            , span [ tibetanText model.largeText ] [ text model.nextGoal ]
            ]
        , p [] [ span [ largeText model.largeText ] [ text ("Current lesson: " ++ (showDifficulty model.difficultyLevel)) ] ]
        , p []
            [ span [ largeText model.largeText ] [ text "Progress towards next lesson: " ]
            , span [ colorAttribute green ] [ span [ largeText model.largeText ] [ text << first <| (progressBar model) ] ]
            , span [ colorAttribute grey ] [ span [ largeText model.largeText ] [ text << second <| (progressBar model) ] ]
            ]
        , displayMessage model.largeText model.maxDifficulty model.difficultyLevel
        , helper model.difficultyLevel model.largeText
        , p [] []
        , img [ attribute "src" "kb.jpg" ] []
        ]
