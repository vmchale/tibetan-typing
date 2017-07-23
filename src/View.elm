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
        ( String.concat <| List.repeat count "-", String.concat <| List.repeat (17 - count) "-" )


pageStyles : Styles
pageStyles =
    [ marginTop "80px", marginLeft "80px", marginRight "160px" ]


{-| First argument is whether the text should be enlarged
-}
tibetanText : Bool -> Attribute Msg
tibetanText b =
    if not b then
        style [ ( "font-size", "140%" ) ]
    else
        style [ ( "font-size", "280%" ) ]


largeText : Bool -> Attribute Msg
largeText b =
    if b then
        style [ ( "font-size", "200%" ) ]
    else
        style []


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


colorFailed : Bool -> Attribute Msg
colorFailed b =
    if b then
        colorAttribute red
    else
        colorAttribute black


helper : Difficulty -> Bool -> Html Msg
helper diff b =
    case diff of
        Consonants ->
            p [] [ span [ largeText b ] [ text "Use the picture below to type consonants." ] ]

        Vowels ->
            p [] [ span [ largeText b ] [ text "Vowel markers always appear modifying a consonant. Type the vowel after the consonant." ] ]

        Words ->
            p [] [ span [ largeText b ] [ text "Words separate units by the '་' character (called a ཚེག). These units consist of a root letter, and possibly a vowel marker, prefix letter, suffix, or second suffix" ] ]

        Subjoined ->
            p [] [ span [ largeText b ] [ text "To subjoin a letter, type the top consonant, followed by the compose key ('m') and then the bottom letter. Type any vowels after that." ] ]

        Phrases ->
            p [] []

        Sentences ->
            p [] [ span [ largeText b ] [ text "Separate units of text know as དོན་ཚན་with a ། (called a ཤད་)" ] ]

        Punctuation ->
            p [] [ span [ largeText b ] [ text "The ཡིང་མགོ་ (༄༅) is used in དཔེ་ཆ་, but it is not used in books." ] ]

        Numerals ->
            p [] []


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
        , helper model.difficultyLevel model.largeText
        , p [] []
        , img [ attribute "src" "kb.jpg" ] []
        ]
