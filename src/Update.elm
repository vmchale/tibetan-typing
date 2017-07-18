module Update exposing (update)

import State exposing (..)
import Update.Lang exposing (..)
import Random exposing (int, Generator)
import Platform.Cmd exposing (none)
import Dict exposing (get)
import Keyboard exposing (KeyCode)
import Maybe exposing (..)
import Char exposing (fromCode)
import List exposing (take, drop, reverse, repeat)
import String exposing (fromChar, uncons, dropLeft)


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


getChar : String -> Char
getChar s =
    case uncons s of
        Just ( c, _ ) ->
            c

        Nothing ->
            'བ'


update : Msg -> Model -> ( Model, Cmd Msg )
update msg st =
    case msg of
        None ->
            st ! []

        KeyMsg i ->
            let
                fail =
                    not (mkKeyPress st.composeNext i == st.nextChar)
            in
                { st
                    | lastKeyPress = Just (mkKeyPress st.composeNext i)
                    , pastSuccesses =
                        let
                            x =
                                (not fail) :: take 19 st.pastSuccesses
                        in
                            if x == repeat 20 True then
                                repeat 20 False
                            else
                                x
                    , completed =
                        st.completed
                            ++ (if (not fail) && (not st.composeNext) then
                                    addCompleted i
                                else
                                    addCompletedSubjoined i
                               )
                    , nextGoal =
                        if (not fail) then
                            dropLeft 1 st.nextGoal
                        else
                            st.nextGoal
                    , nextChar =
                        if (not fail) then
                            getChar st.nextGoal
                        else
                            st.nextChar
                    , failed = fail && not (mkKeyPress st.composeNext i == 'M')
                    , difficultyLevel =
                        let
                            x =
                                (not fail) :: take 19 st.pastSuccesses
                        in
                            if x == repeat 20 True then
                                succ st.difficultyLevel
                            else
                                st.difficultyLevel
                    , composeNext = mkKeyPress st.composeNext i == 'M'
                }
                    ! []
