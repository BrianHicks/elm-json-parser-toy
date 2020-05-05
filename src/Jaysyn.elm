module Jaysyn exposing (Jaysyn(..), fromString)

import Parser exposing ((|.), (|=), Parser)


type Jaysyn
    = Null
    | Bool Bool
    | Float Float
    | Int Int
    | String String
    | Array (List Jaysyn)
    | Object (List ( String, Jaysyn ))


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

        -- strings
        , Parser.succeed String
            |. Parser.token "\""
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"'))
            |. Parser.token "\""

        -- arrays
        , Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = Parser.spaces
            , item = Parser.lazy (\_ -> parser)
            , trailing = Parser.Optional
            }
            |> Parser.map Array

        -- objects
        , Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = Parser.spaces
            , item = objectItem
            , trailing = Parser.Optional
            }
            |> Parser.map Object
        ]


objectItem : Parser ( String, Jaysyn )
objectItem =
    Parser.succeed Tuple.pair
        |. Parser.token "\""
        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"'))
        |. Parser.token "\""
        |. Parser.spaces
        |. Parser.token ":"
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)
