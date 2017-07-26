module Update.Keys exposing (..)

import String exposing (split, uncons)
import Char exposing (KeyCode, toCode)
import List exposing (map, map2, foldr)
import Array exposing (Array, fromList)
import Dict exposing (Dict)


c : String -> Char
c s =
    case uncons s of
        Just ( x, _ ) ->
            x

        Nothing ->
            '?'


subjoinedCharsLatin : List Char
subjoinedCharsLatin =
    map c <| split "" "9C9Y9L9R"


subjoinedChars : List Char
subjoinedChars =
    map c <| split "" "སྐཀྱཀླཀྲ"


tibChars : List Char
tibChars =
    map c <| split "" "ཨཧཝཅཆེརཏཡིོུཕཙཚཛའསདབངམ་གལཞ།ཟཤཀཁཔནཐཇཉ"


latinChars : List Char
latinChars =
    map c <| split "" "`-=QWERTYUIOP[]\\ASDFGHJKL;'ZXCVBN,./"


helper : Char -> KeyCode
helper c =
    case c of
        '[' ->
            219

        ']' ->
            221

        '\\' ->
            220

        ';' ->
            186

        '\'' ->
            222

        '/' ->
            191

        ',' ->
            188

        '=' ->
            187

        '-' ->
            189

        '`' ->
            192

        '.' ->
            190

        _ ->
            toCode c


keymapSubjoined : Dict KeyCode Char
keymapSubjoined =
    Dict.fromList <|
        map (\( char, c ) -> ( helper char, c )) <|
            map2 (,) subjoinedCharsLatin subjoinedChars


keymap : Dict KeyCode Char
keymap =
    Dict.fromList <|
        map (\( char, c ) -> ( helper char, c )) <|
            map2 (,) latinChars tibChars
