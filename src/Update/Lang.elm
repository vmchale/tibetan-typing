module Update.Lang exposing (..)

import String exposing (split, uncons)
import Char exposing (KeyCode, toCode)
import List exposing (map, map2, foldr)
import Array exposing (Array, fromList, append)
import Dict exposing (Dict)


consonants : Array String
consonants =
    fromList <| split "" "ཨཧཝཅཆརཏཡཕཙཚཛའསདབངམགལཞཟཤཀཁཔནཐཇཉ"


yatak : List String
yatak =
    [ "ཀྱ", "གྱ", "ཁྱ", "པྱ", "ཕྱ", "བྱ", "མྱ" ]


latak : List String
latak =
    [ "ཀླ", "གླ", "བླ", "ཟླ", "རླ", "སླ" ]


ratak : List String
ratak =
    [ "ཀྲ", "ཁྲ", "གྲ", "དྲ", "ནྲ", "ཕྲ", "པྲ", "བྲ", "མྲ", "ཤྲ", "སྲ", "ཧྲ" ]


ragoyatak : List String
ragoyatak =
    [ "རྐྱ", "རྒྱ", "རྨྱ" ]


sagoratak : List String
sagoratak =
    [ "སྐྲ", "སྒྲ", "སྤྲ", "སྦྲ", "སྨྲ" ]


sagoyatak : List String
sagoyatak =
    [ "སྐྱ", "སྒྱ", "སྤྱ", "སྦྱ", "སྨྱ" ]


rago : List String
rago =
    [ "རྐ", "རྒ", "རྔ", "རྗ", "རྙ", "རྟ", "རྡ", "རྣ", "རྣ", "རྦ", "རྨ", "རྩ", "རྫ" ]


sago : List String
sago =
    [ "སྐ", "སྒ", "སྔ", "སྙ", "སྟ", "སྡ", "སྣ", "སྤ", "སྦ", "སྨ", "སྩ" ]


lago : List String
lago =
    [ "ལྐ", "ལྒ", "ལྔ", "ལྕ", "ལྗ", "ལྟ", "ལྡ", "ལྤ", "ལྦ" ]


wazur : List String
wazur =
    [ "ཀྭ", "ཁྭ", "གྭ", "ཉྭ", "དྭ", "ཚྭ", "ཞྭ", "ཟྭ", "རྭ", "ལྭ", "ཤྭ", "ཧྭ" ]


subjoinedEasy : Array String
subjoinedEasy =
    fromList subjoinedEasyL


subjoinedEasyL : List String
subjoinedEasyL =
    foldr (++) (ratak) [ yatak, latak, rago, lago, sago ]


words : Array String
words =
    fromList [ "བོད་", "ཁྱེད་རང་", "ངའི", "ཡོད་", "སོང་", "རེད་" ]


vowels : Array String
vowels =
    fromList [ "ཨ", "ཨེ", "ཨི", "ཨུ", "ཨོ", "ཧ", "ཧེ", "ཧི", "ཧུ", "ཧོ", "ཅ", "ཅེ", "ཅི", "ཅོ", "ཅུ", "ཆ", "ཆེ", "ཆི", "ཆོ", "ཆུ", "ར", "རེ", "རི", "རོ", "རུ", "ཏ", "ཏེ", "ཏི", "ཏོ", "ཏུ", "ཡ", "ཡེ", "ཡུ", "ཡི", "ཡོ", "ཕ", "ཕེ", "ཕི", "ཕུ", "ཕོ", "ཙ", "ཙེ", "ཙི", "ཙོ", "ཙུ", "ཚ", "ཚེ", "ཚི", "ཚོ", "ཚུ", "ཛ", "ཛེ", "ཛི", "ཛོ", "ཛུ", "འ", "འེ", "འི", "འོ", "འུ", "ས", "སེ", "སི", "སོ", "སུ", "ད", "དེ", "དི", "དོ", "དུ", "བ", "བེ", "བི", "བོ", "བུ", "ང", "ངེ", "ངི", "ངོ", "ངུ", "མ", "མེ", "མི", "མོ", "མུ", "ག", "གེ", "གི", "གོ", "གུ", "ལ", "ལེ", "ལི", "ལོ", "ལུ", "ཞ", "ཞེ", "ཞི", "ཞོ", "ཞུ", "ཟ", "ཟེ", "ཟི", "ཟོ", "ཟུ", "ཤ", "ཤེ", "ཤི", "ཤོ", "ཤུ", "ཀ", "ཀེ", "ཀི", "ཀོ", "ཀུ", "ཁ", "ཁེ", "ཁི", "ཁོ", "ཁུ", "པ", "པེ", "པི", "པོ", "པུ", "ན", "ནེ", "ནི", "ནོ", "ནུ", "ཐ", "ཐེ", "ཐི", "ཐོ", "ཐུ", "ཇ", "ཇེ", "ཇི", "ཇོ", "ཇུ", "ཉ", "ཉེ", "ཉི", "ཉོ", "ཉུ" ]


punctuation : Array String
punctuation =
    fromList [ "་", "།" ]


numerals : Array String
numerals =
    fromList <| split "" "༠༡༢༣༤༥༦༧༨༩"


phrases : Array String
phrases =
    fromList [ "ཨ་རི་ནས་" ]


sentences : Array String
sentences =
    fromList [ "་བོད་སྐད་ལ་", "ང་ན་སོང།", "ཁྱེད་རང་མིང་ག་ར་ཡིན།", "ང་ཨ་རི་ནས་ཡིན།" ]



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
