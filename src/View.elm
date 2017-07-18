module View exposing (view)

import Html exposing (..)
import State exposing (..)
import Color exposing (..)
import Color.Convert exposing (colorToHex)
import Html.Attributes exposing (style)
import Html exposing (img)
import Style exposing (..)
import List exposing (..)
import String exposing (concat, fromChar)
import Tuple exposing (first, second)
import Json.Decode as Json
import Html.Events exposing (on, keyCode)
import Html.Attributes exposing (attribute)


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


helper : Difficulty -> Html Msg
helper diff =
    case diff of
        Consonants ->
            p [] [ text "Use the chart below to type consonants." ]

        Vowels ->
            p []
                [ text "Vowel markers always appear modifying a consonant. Type the vowel after the consonant."
                ]

        Words ->
            p [] [ text "Words separate units by the '་' character (called a ཚེག). These units consist of a root letter, and possibly a vowel marker, prefix letter, suffix, or second suffix" ]

        Subjoined ->
            p [] [ text "To subjoin a letter, type the top consonant, followed by the compose key ('∘') and then the bottom letter. Type any vowels after that." ]

        Phrases ->
            p [] []

        Sentences ->
            p [] [ text "Separate units of text know as དོན་ཚན་with a ། (called a ཤད་)" ]

        Punctuation ->
            p [] [ text "The ཡིང་མགོ་ (༄༅) is used in དཔེ་ཆ་, but it is not used in books." ]

        Numerals ->
            p [] []


view : Model -> Html Msg
view model =
    div [ style pageStyles ]
        [ p [] [ text "Type the following: " ]
        , p []
            [ span [ colorAttribute green, tibetanText ] [ text model.completed ]
            , span [ colorFailed model.failed, tibetanText ] [ text << fromChar <| model.nextChar ]
            , span [ tibetanText ]
                [ if String.length model.nextGoal > 1 then
                    text model.nextGoal
                  else
                    text ""
                ]
            ]
        , p [] [ text ("Current lesson: " ++ (showDifficulty model.difficultyLevel)) ]
        , p []
            [ text "Progress towards next lesson: "
            , span [ colorAttribute green ] [ text << first <| (progressBar model) ]
            , span [ colorAttribute grey ] [ text << second <| (progressBar model) ]
            ]
        , helper model.difficultyLevel
        , p [] []
        , img [ attribute "src" "kb.jpg" ] []
        ]
