module View.Content exposing (..)

import Html exposing (..)
import State exposing (..)
import Color exposing (..)
import Color.Convert exposing (colorToHex)
import Html.Events exposing (onClick)
import List exposing (..)
import Update exposing (levelNum)
import View.Styles exposing (..)


helper : Difficulty -> Bool -> Html Msg
helper diff b =
    case diff of
        Consonants ->
            p [] [ span [ largeText b ] [ text "Use the picture below to type consonants." ] ]

        Vowels ->
            p [] [ span [ largeText b ] [ text "Vowel markers always appear modifying a consonant. Type the vowel after the consonant." ] ]

        Words ->
            p [] [ span [ largeText b ] [ text "Words separate units by the '་' character (called a ཚེག). These units consist of a root letter, and possibly a vowel marker, prefix letter, suffix, or second suffix" ] ]

        SubjoinedEasy ->
            p [] [ span [ largeText b ] [ text "To subjoin a letter, type the top consonant, followed by the compose key ('m') and then the bottom letter. Type any vowels after that." ] ]

        Subjoined ->
            p [] [ span [ largeText b ] [ text "Letters can have both a superscript and subscript adjoined (and sometimes even a vowel!)" ] ]

        Phrases ->
            p [] []

        Sentences ->
            p [] [ span [ largeText b ] [ text "Separate units of text know as དོན་ཚན་with a ། (called a ཤད་)" ] ]

        Punctuation ->
            p [] [ span [ largeText b ] [ text "The ཡིང་མགོ་ (༄༅) is used in དཔེ་ཆ་, but it is not used in books." ] ]

        Numerals ->
            p [] []


allDifficulties : List Difficulty
allDifficulties =
    [ Consonants, Vowels, SubjoinedEasy, Subjoined, Words, Phrases, Sentences, Punctuation, Numerals ]


displayMessage : Bool -> Difficulty -> Html Msg
displayMessage b difficulty =
    div []
        [ span [ largeText b ] [ text "Available Lessons: " ]
        , span [ largeText b ] (intersperse (text " | ") <| (map2 (\diff str -> span [ onClick (SetDifficulty diff) ] [ text str ]) allDifficulties (List.map showDifficulty (filter (\d -> toInt d <= toInt difficulty) allDifficulties))))
        ]


progressBar : Model -> ( String, String )
progressBar model =
    let
        past =
            take (levelNum model.difficultyLevel) model.pastSuccesses

        boolInt : Bool -> Int
        boolInt b =
            if b then
                1
            else
                0

        count =
            sum <| List.map boolInt past
    in
        ( String.concat <| List.repeat count "-", String.concat <| List.repeat ((levelNum model.difficultyLevel) * 17 // 20 - count) "-" )
