module Update exposing (update)

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


enoughProgress : List Bool -> Bool
enoughProgress l =
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
        >= 17


getArray : Difficulty -> Array String
getArray d =
    case d of
        Consonants ->
            consonants

        Vowels ->
            vowels

        _ ->
            subjoined


update : Msg -> Model -> ( Model, Cmd Msg )
update msg st =
    case msg of
        None ->
            st ! []

        RandomString s ->
            { st | nextGoal = dropLeft 1 s, nextChar = getChar s, failed = False } ! []

        KeyMsg i ->
            let
                fail =
                    not (mkKeyPress st.composeNext i == st.nextChar)

                done =
                    (not fail)
                        && String.length st.nextGoal
                        == 0
            in
                { st
                    | lastKeyPress = Just (mkKeyPress st.composeNext i)
                    , pastSuccesses =
                        let
                            x =
                                (not fail) :: take 19 st.pastSuccesses
                        in
                            if enoughProgress x then
                                repeat 20 False
                            else
                                x
                    , completed =
                        if not done then
                            st.completed
                                ++ (if (not fail) && (not st.composeNext) then
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
                    , difficultyLevel =
                        let
                            x =
                                (not fail) :: take 19 st.pastSuccesses
                        in
                            if enoughProgress x then
                                succ st.difficultyLevel
                            else
                                st.difficultyLevel
                    , composeNext = mkKeyPress st.composeNext i == 'M'
                }
                    ! (if (not done) then
                        []
                       else
                        [ getNext << getArray <| st.difficultyLevel ]
                      )
