module Update.Lang exposing (..)

import String exposing (split, uncons)
import Char exposing (KeyCode, toCode)
import List exposing (map, map2)
import Array exposing (Array, fromList, append)
import Dict exposing (Dict)


consonants : Array String
consonants =
    fromList <| split "" "ཨཧཝཅཆརཏཡཕཙཚཛའསདབངམགལཞཟཤཀཁཔནཐཇཉ"


yatak : Array String
yatak =
    fromList [ "ཀྱ", "གྱ", "ཁྱ", "དྱ", "པྱ", "ཕྱ", "བྱ", "མྱ" ]


latak : Array String
latak =
    fromList [ "ཀླ", "གླ", "བླ", "ཟླ", "རླ", "སླ" ]


ratak : Array String
ratak =
    fromList [ "ཀྲ", "ཁྲ", "གྲ", "དྲ", "ནྲ", "ཕྲ", "པྲ", "བྲ", "མྲ", "ཤྲ", "སྲ", "ཧྲ" ]


subjoined : Array String
subjoined =
    append yatak (append latak ratak)


vowels : Array String
vowels =
    fromList [ "ཨ", "ཨེ", "ཨི", "ཨུ", "ཨོ", "ཧ", "ཧེ", "ཧི", "ཧུ", "ཧོ" ]


numeral : Array String
numeral =
    fromList <| split "" "༠༡༢༣༤༥༦༧༨༩"


sentences : Array String
sentences =
    fromList [ "་བོད་སྐད་ལ་" ]



-- We need to map the keycode to a Tibetan letter/numeral.
-- This allows users to type in Tibetan even without installing any special keyboards.
-- Ideally we'd have a nice flashing keyboard at the bottom too?


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
