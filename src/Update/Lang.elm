module Update.Lang exposing (..)

import String exposing (split, dropLeft)
import Array exposing (Array, fromList, append, foldr)
import Dict exposing (Dict)


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
    fromList [ "རྐ", "རྒ", "རྔ", "རྗ", "རྙ", "རྟ", "རྡ", "རྣ", "རྣ", "རྦ", "རྨ", "རྩ", "རྫ" ]


sago : Array String
sago =
    fromList [ "སྐ", "སྒ", "སྔ", "སྙ", "སྟ", "སྡ", "སྣ", "སྤ", "སྦ", "སྨ", "སྩ" ]


lago : Array String
lago =
    fromList [ "ལྐ", "ལྒ", "ལྔ", "ལྕ", "ལྗ", "ལྟ", "ལྡ", "ལྤ", "ལྦ" ]



-- TODO wazur can appear as a second subscript, e.g. གྲྭ་


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
