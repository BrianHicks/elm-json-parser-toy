module Jaysyn exposing (Jaysyn(..), fromString)

import Parser exposing ((|.), (|=), Parser)


type Jaysyn
    = Null
    | Bool Bool
    | Float Float
    | Int Int


fromString : String -> Result (List Parser.DeadEnd) Jaysyn
fromString source =
    Parser.run parser source


parser : Parser Jaysyn
parser =
    Parser.oneOf
        [ Parser.map (\_ -> Null) (Parser.keyword "null")

        -- booleans
        , Parser.map (\_ -> Bool True) (Parser.keyword "true")
        , Parser.map (\_ -> Bool False) (Parser.keyword "false")

        -- numbers
        , Parser.succeed identity
            |. Parser.token "-"
            |= Parser.number
                { int = Just (\i -> Int -i)
                , hex = Just (\i -> Int -i)
                , octal = Nothing
                , binary = Nothing
                , float = Just (\i -> Float -i)
                }
        , Parser.number
            { int = Just Int
            , hex = Just Int
            , octal = Nothing
            , binary = Nothing
            , float = Just Float
            }
        ]
