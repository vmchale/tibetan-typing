module Update.Lang exposing (..)

import String exposing (split, dropLeft)
import Array exposing (Array, fromList, append, foldr)


consonants : Array String
consonants =
    fromList <| split "" "ཨཧཝཅཆརཏཡཕཙཚཛའསདབངམགལཞཟཤཀཁཔནཐཇཉ"


yatak : Array String
yatak =
    fromList [ "ཀྱ", "གྱ", "ཁྱ", "པྱ", "ཕྱ", "བྱ", "མྱ" ]


latak : Array String
latak =
    fromList [ "ཀླ", "གླ", "བླ", "ཟླ", "རླ", "སླ" ]


ratak : Array String
ratak =
    fromList [ "ཀྲ", "ཁྲ", "གྲ", "དྲ", "ནྲ", "ཕྲ", "པྲ", "བྲ", "མྲ", "ཤྲ", "སྲ", "ཧྲ" ]


ragoyatak : Array String
ragoyatak =
    fromList [ "རྐྱ", "རྒྱ", "རྨྱ" ]


sagoratak : Array String
sagoratak =
    fromList [ "སྐྲ", "སྒྲ", "སྤྲ", "སྦྲ", "སྨྲ" ]


sagoyatak : Array String
sagoyatak =
    fromList [ "སྐྱ", "སྒྱ", "སྤྱ", "སྦྱ", "སྨྱ" ]


rago : Array String
rago =
    fromList [ "རྐ", "རྒ", "རྔ", "རྗ", "རྙ", "རྟ", "རྡ", "རྣ", "རྦ", "རྨ", "རྩ", "རྫ" ]


sago : Array String
sago =
    fromList [ "སྐ", "སྒ", "སྔ", "སྙ", "སྟ", "སྡ", "སྣ", "སྤ", "སྦ", "སྨ", "སྩ" ]


lago : Array String
lago =
    fromList [ "ལྐ", "ལྒ", "ལྔ", "ལྕ", "ལྗ", "ལྟ", "ལྡ", "ལྤ", "ལྦ" ]



-- TODO wazur can appear as a second subscript, e.g. གྲྭ་
-- we make a list of all such common second subscripts


wazur : Array String
wazur =
    fromList [ "ཀྭ", "ཁྭ", "གྭ", "ཉྭ", "དྭ", "ཚྭ", "ཞྭ", "ཟྭ", "རྭ", "ལྭ", "ཤྭ", "ཧྭ" ]


subjoined : Array String
subjoined =
    append subjoinedEasy subjoinedEasy


subjoinedFolded : Array String
subjoinedFolded =
    foldr append ragoyatak <| fromList [ sagoratak, sagoyatak ]


subjoinedEasy : Array String
subjoinedEasy =
    foldr append ratak <| fromList [ yatak, latak, rago, lago, sago ]


words : Array String
words =
    fromList [ "བོད་", "ཁྱེད་རང་", "ངའི", "ཡོད་", "སོང་", "རེད་" ]


vowelModifiers : Array String
vowelModifiers =
    Array.map (dropLeft 1) <| fromList [ "འ", "འེ", "འི", "འོ", "འུ" ]


punctuation : Array String
punctuation =
    fromList [ "་", "།" ]


numerals : Array String
numerals =
    fromList <| split "" "༠༡༢༣༤༥༦༧༨༩"


phrases : Array String
phrases =
    fromList [ "བོད་སྐད་ལ་", "ཨ་རི་ནས་" ]


headlines : Array String
headlines =
    fromList [ "སྲིད་འཛིན་ཊ་རམཕྷ་དང་ ར་ཤིའི་དབར་འབྲེལ་བའི་རྙོག་གཞི།", "ཨ་རི་དང་རྒྱ་ནག་གི་ཚོང་ལས་རྩོད་གཞི་ཡོད་པ་གྲོས་མོལ་བྱས་སྟེ་སེལ་དགོས་པ།", "ཨ་རི་དང་ར་ཤི་ཡའི་སྲིད་འཛིན་གཉིས་པར་གྱི་འབྲེལ་བ", "རྒྱ་ནག་གིས་རྒྱ་གར་ལ་གླགས་འཚོལ་མ་བྱ་ཞེས་ཉེན་བརྡ་བཏང་བ།" ]


sentences : Array String
sentences =
    fromList [ "ང་ན་སོང།", "ཁྱེད་རང་གི་མིང་ག་ར་ཡིན།", "ང་ཨ་རི་ནས་ཡིན།" ]
