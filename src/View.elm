module View exposing (view)

import Html exposing (..)
import State exposing (..)
import Color exposing (..)
import Color.Convert exposing (colorToHex)
import Html.Attributes exposing (style)
import Style exposing (..)
import List exposing (..)
import String exposing (concat, fromChar)
import Tuple exposing (first, second)
import Json.Decode as Json
import Html.Events exposing (on, keyCode)


type alias Styles =
    List Style


colorAttribute : Color -> Attribute Msg
colorAttribute color =
    style [ ( "color", colorToHex color ) ]


progressBar : Model -> ( String, String )
progressBar model =
    let
        past20 =
            take 20 model.pastSuccesses

        boolInt : Bool -> Int
        boolInt b =
            if b then
                1
            else
                0

        count =
            sum <| List.map boolInt past20
    in
        ( String.concat <| List.repeat count "-", String.concat <| List.repeat (20 - count) "-" )


pageStyles : Styles
pageStyles =
    [ marginTop "80px", marginLeft "80px" ]


tibetanText : Attribute Msg
tibetanText =
    style [ ( "font-size", "140%" ) ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


colorFailed : Bool -> Attribute Msg
colorFailed b =
    if b then
        colorAttribute red
    else
        colorAttribute black


{-| This view needs to display the current goal, progress towards that goal, and whether the last keypress
was wrong. Also some kind of progress bar to determine whether to progress to the next level?
-}
view : Model -> Html Msg
view model =
    div [ style pageStyles ]
        [ p [] [ text "Type the following: " ]
        , p []
            [ span [ colorAttribute green, tibetanText ] [ text model.completed ]
            , span [ colorFailed model.failed, tibetanText ] [ text << fromChar <| model.nextChar ]
            , span [ tibetanText ] [ text model.nextGoal ]
            ]
        , p [] [ text ("Current lesson: " ++ (showDifficulty model.difficultyLevel)) ]
        , p []
            [ text "Progress towards next lesson: "
            , span [ colorAttribute green ] [ text << first <| (progressBar model) ]
            , span [ colorAttribute grey ] [ text << second <| (progressBar model) ]
            ]
        ]
