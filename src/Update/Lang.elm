module Update.Lang exposing (..)

import String exposing (split)
import Char exposing (KeyCode, toCode)
import List exposing (map)
import Array exposing (Array, fromList)
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


numeral : Array String
numeral =
    fromList <| split "" "༠༡༢༣༤༥༦༧༨༩"



-- We need to map the keycode to a Tibetan letter/numeral.
-- This allows users to type in Tibetan even without installing any special keyboards.
-- Ideally we'd have a nice flashing keyboard at the bottom too?


keymap : Dict KeyCode Char
keymap =
    Dict.fromList <|
        map (\( char, c ) -> ( toCode char, c ))
            [ ( '`', 'ཨ' ), ( 'F', 'བ' ) ]
