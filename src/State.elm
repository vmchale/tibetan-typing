module State exposing (..)

import Platform.Cmd exposing (none)
import Keyboard exposing (KeyCode, downs)


type Difficulty
    = Consonants
    | Vowels
    | Subjoined
    | Words
    | Phrases
    | Sentences
    | Punctuation
    | Numerals


type Msg
    = None
    | KeyMsg KeyCode
    | RandomString String
    | SetDifficulty Difficulty


succ : Difficulty -> Difficulty
succ d =
    case d of
        Consonants ->
            Vowels

        Vowels ->
            Subjoined

        Subjoined ->
            Words

        Words ->
            Phrases

        Phrases ->
            Sentences

        _ ->
            Sentences


toInt : Difficulty -> Int
toInt d =
    case d of
        Consonants ->
            1

        Vowels ->
            2

        Subjoined ->
            3

        Words ->
            4

        Phrases ->
            5

        Sentences ->
            6

        Punctuation ->
            7

        Numerals ->
            7


showDifficulty : Difficulty -> String
showDifficulty diff =
    case diff of
        Consonants ->
            "Consonants"

        Vowels ->
            "Vowels"

        Subjoined ->
            "Subjoined Letters"

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


type alias Model =
    { nextGoal : String -- the chunk of the string to be completed after the current char
    , completed : String -- the chunk of the string already completed
    , nextChar : Char -- the character they need to type next
    , failed : Bool -- whether they pressed the wrong key
    , composeNext : Bool -- when the 'm' key is pressed, get ready for some subjoined letters.
    , lastKeyPress : Maybe Char -- last key press in case we need to compose something
    , difficultyLevel : Difficulty -- e.g. consonants, subjoined letters, words, etc.
    , maxDifficulty : Difficulty -- so you don't get bumped to lower levels permanently
    , pastSuccesses : List Bool -- for determining whether to progress to the next level
    , largeText : Bool -- whether to display large text for accessibility purposes.
    }


init =
    ( Model "" "" 'ང' False False Nothing Consonants Consonants [] False, none )
