module State exposing (..)

import Platform.Cmd exposing (none, batch)
import State.Types exposing (..)


succ : Difficulty -> Difficulty
succ d =
    case d of
        Consonants ->
            Vowels

        Vowels ->
            SubjoinedEasy

        SubjoinedEasy ->
            Subjoined

        Subjoined ->
            Words

        Words ->
            Phrases

        _ ->
            Sentences


toInt : Difficulty -> Int
toInt d =
    case d of
        Consonants ->
            1

        Vowels ->
            2

        SubjoinedEasy ->
            3

        Subjoined ->
            4

        Words ->
            5

        Phrases ->
            6

        Sentences ->
            7

        Punctuation ->
            8

        Numerals ->
            8


fromString : String -> Difficulty
fromString str =
    case str of
        "Consonants" ->
            Consonants

        "Vowels" ->
            Vowels

        "Subjoined Letters" ->
            SubjoinedEasy

        "Folded Letters" ->
            Subjoined

        "Words" ->
            Words

        "Phrases" ->
            Phrases

        "Sentences" ->
            Sentences

        "Punctuation & Typographical Marks" ->
            Punctuation

        "Tibetan Numerals" ->
            Numerals

        _ ->
            Consonants


showDifficulty : Language -> Difficulty -> String
showDifficulty _ d =
    showDifficultyEnglish d


showDifficultyBo : Difficulty -> String
showDifficultyBo diff =
    case diff of
        Consonants ->
            "གསལ་བྱེད་"

        Vowels ->
            "དབྱངས་"

        -- FIXME subscripts etc.
        SubjoinedEasy ->
            "ར་མགོ་ཅན"

        Words ->
            "མིང་ཚིག"

        Sentences ->
            "ཚིག་རྐད་།"

        _ ->
            ""


showDifficultyEnglish : Difficulty -> String
showDifficultyEnglish diff =
    case diff of
        Consonants ->
            "Consonants"

        Vowels ->
            "Vowels"

        SubjoinedEasy ->
            "Subjoined Letters"

        Subjoined ->
            "Folded Letters"

        Words ->
            "Words"

        Phrases ->
            "Phrases"

        Sentences ->
            "Sentences"

        Punctuation ->
            "Punctuation & Typographical Marks"

        Numerals ->
            "Tibetan Numerals"


init : Maybe String -> ( Model, Cmd Msg )
init savedState =
    case savedState of
        Just str ->
            ( Model "" "" 'ང' False False Nothing Consonants (fromString str) [] False English, none )

        Nothing ->
            ( Model "" "" 'ང' False False Nothing Consonants Consonants [] False English, none )
