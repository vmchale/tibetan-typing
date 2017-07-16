module State exposing (..)

import Platform.Cmd exposing (none)
import Keyboard exposing (KeyCode, downs)


-- TODO call this 'Lesson' instead?


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


{-| Function to show the current difficulty level for the user.
-}
showDifficulty : Difficulty -> String
showDifficulty diff =
    case diff of
        Consonants ->
            "Consonants"

        Vowels ->
            "Consonants & Vowels"

        Subjoined ->
            "Subjoined & Superscript Letters"

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
    , pastSuccesses : List Bool -- for determining whether to progress to the next level
    }


init =
    ( Model "་བོད་སྐད་ལ་" "" 'ང' False False Nothing Consonants [], none )
