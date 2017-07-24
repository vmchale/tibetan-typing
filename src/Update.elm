module Update exposing (update, levelNum)

import State exposing (..)
import Update.Lang exposing (..)
import Random exposing (int, Generator)
import Platform.Cmd exposing (none, Cmd)
import Dict exposing (get)
import Keyboard exposing (KeyCode)
import Maybe exposing (..)
import Char exposing (fromCode)
import List exposing (take, drop, reverse, repeat, foldr)
import String exposing (fromChar, uncons, dropLeft)
import Random exposing (int, map, generate)
import Array exposing (Array, length)


addCompleted : Int -> String
addCompleted i =
    case get i keymap of
        Just c ->
            fromChar c

        _ ->
            ""


addCompletedSubjoined : Int -> String
addCompletedSubjoined i =
    case get i keymapSubjoined of
        Just c ->
            fromChar c

        _ ->
            ""


mkKeyPress : Bool -> Int -> Char
mkKeyPress subjoined i =
    case subjoined of
        False ->
            withDefault (fromCode i) <| get i keymap

        True ->
            withDefault (fromCode i) <| get i keymapSubjoined


const : a -> (b -> a)
const a =
    (\x -> a)


fromMaybe : a -> Maybe a -> a
fromMaybe def val =
    case val of
        Just v ->
            v

        Nothing ->
            def


getNext : Array String -> Cmd Msg
getNext ls =
    generate (\i -> RandomString << fromMaybe "Error" << (Array.get i) <| ls) (int 0 (length ls - 1))


getChar : String -> Char
getChar s =
    case uncons s of
        Just ( c, _ ) ->
            c

        Nothing ->
            'བ'


{-| Takes the past record and the current lesson, returns 'True' (i.e. we can progress)
when the user has hit 85% of the past keys correctly.
-}
enoughProgress : List Bool -> Difficulty -> Bool
enoughProgress l d =
    (foldr
        (\b n ->
            if b then
                n + 1
            else
                n
        )
        0
        l
    )
        >= (17 * levelNum d)
        // 20


{-| Set the number of correct keypresses required to progress to the next level
-}
levelNum : Difficulty -> Int
levelNum d =
    case d of
        Consonants ->
            20

        Vowels ->
            40

        Subjoined ->
            30

        Words ->
            30

        Phrases ->
            60

        Sentences ->
            200

        _ ->
            100


getArray : Difficulty -> Array String
getArray d =
    case d of
        Consonants ->
            consonants

        Vowels ->
            vowels

        Subjoined ->
            subjoined

        Words ->
            words

        Phrases ->
            phrases

        Sentences ->
            sentences

        Numerals ->
            numerals

        Punctuation ->
            punctuation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg st =
    case msg of
        None ->
            st ! []

        SetDifficulty d ->
            { st | difficultyLevel = d } ! []

        RandomString s ->
            { st | nextGoal = dropLeft 1 s, nextChar = getChar s, failed = False } ! []

        KeyMsg 113 ->
            { st | largeText = not st.largeText } ! []

        KeyMsg i ->
            let
                fail =
                    not (mkKeyPress st.composeNext i == st.nextChar)

                done =
                    (not fail)
                        && String.length st.nextGoal
                        == 0

                recordNum =
                    levelNum st.difficultyLevel

                pastProgress =
                    (not fail) :: take (recordNum - 1) st.pastSuccesses

                newDifficultyLevel =
                    if enoughProgress pastProgress st.difficultyLevel then
                        succ st.difficultyLevel
                    else
                        st.difficultyLevel
            in
                { st
                    | lastKeyPress = Just (mkKeyPress st.composeNext i)
                    , pastSuccesses =
                        let
                            x =
                                (not fail) :: take (recordNum - 1) st.pastSuccesses
                        in
                            if enoughProgress x st.difficultyLevel then
                                repeat recordNum False
                            else if not (mkKeyPress st.composeNext i == 'M') then
                                x
                            else
                                st.pastSuccesses
                    , completed =
                        if not done then
                            st.completed
                                ++ (if (not fail) then
                                        if (not st.composeNext) then
                                            addCompleted i
                                        else
                                            addCompletedSubjoined i
                                    else
                                        ""
                                   )
                        else
                            ""
                    , nextGoal =
                        if (not fail) then
                            dropLeft 1 st.nextGoal
                        else if not done then
                            st.nextGoal
                        else
                            ""
                    , nextChar =
                        if (not fail) && not done then
                            getChar st.nextGoal
                        else if not done then
                            st.nextChar
                        else
                            ' '
                    , failed = fail && not (mkKeyPress st.composeNext i == 'M')
                    , difficultyLevel = newDifficultyLevel
                    , maxDifficulty =
                        if toInt newDifficultyLevel >= toInt st.difficultyLevel then
                            st.maxDifficulty
                        else
                            newDifficultyLevel
                    , composeNext = mkKeyPress st.composeNext i == 'M'
                }
                    ! (if (not done) then
                        []
                       else
                        [ getNext << getArray <| st.difficultyLevel ]
                      )
