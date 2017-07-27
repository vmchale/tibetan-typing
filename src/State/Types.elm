module State.Types exposing (..)

import Keyboard exposing (KeyCode)


type Language
    = English
    | Tibetan


type Difficulty
    = Consonants
    | Vowels
    | SubjoinedEasy
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
    , helpLanguage : Language -- language to display helper prompts in
    }
