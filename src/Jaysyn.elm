module Jaysyn exposing (Jaysyn(..), fromString)

import Parser exposing ((|.), (|=), Parser)


type Jaysyn
    = Null
    | Bool Bool


fromString : String -> Result (List Parser.DeadEnd) Jaysyn
fromString source =
    Parser.run parser source


parser : Parser Jaysyn
parser =
    Parser.oneOf
        [ Parser.map (\_ -> Null) (Parser.keyword "null")
        , Parser.map (\_ -> Bool True) (Parser.keyword "true")
        , Parser.map (\_ -> Bool False) (Parser.keyword "false")
        ]
