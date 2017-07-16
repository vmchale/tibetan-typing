module Update exposing (update)

import State exposing (..)
import Update.Lang exposing (..)
import Random exposing (int, Generator)
import Platform.Cmd exposing (none)
import Dict exposing (get)
import Keyboard exposing (KeyCode)
import Maybe exposing (..)
import Char exposing (fromCode)
import List exposing (take, drop)
import String exposing (fromChar, uncons, dropLeft)


--getNext : Difficulty -> Generator String


addCompleted : Int -> String
addCompleted i =
    case get i keymap of
        Just c ->
            fromChar c

        _ ->
            ""


mkKeyPress : Int -> Char
mkKeyPress i =
    withDefault (fromCode i) <| get i keymap


getChar : String -> Char
getChar s =
    case uncons s of
        Just ( c, _ ) ->
            c

        Nothing ->
            '༂'


update : Msg -> Model -> ( Model, Cmd Msg )
update msg st =
    case msg of
        None ->
            st ! []

        KeyMsg i ->
            let
                fail =
                    not (mkKeyPress i == st.nextChar)
            in
                { st
                    | lastKeyPress = Just (mkKeyPress i)
                    , pastSuccesses = (not fail) :: take 19 st.pastSuccesses
                    , completed =
                        st.completed
                            ++ (if (not fail) then
                                    addCompleted i
                                else
                                    ""
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
                    , failed = fail
                }
                    ! []
