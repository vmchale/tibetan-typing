module State exposing (..)

import Platform.Cmd exposing (none, batch)
import Keyboard exposing (KeyCode, downs)
import Array exposing (Array, length)
import Task exposing (Task)


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


fromString : String -> Difficulty
fromString str =
    case str of
        "Consonants" ->
            Consonants

        "Vowels" ->
            Vowels

        "Subjoined Letters" ->
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


init : Maybe String -> ( Model, Cmd Msg )
init savedState =
    case savedState of
        Just str ->
            ( Model "" "" 'ང' False False Nothing Consonants (fromString str) [] False, none )

        Nothing ->
            ( Model "" "" 'ང' False False Nothing Consonants Consonants [] False, none )
