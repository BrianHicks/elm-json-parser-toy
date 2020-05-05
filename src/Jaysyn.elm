module Jaysyn exposing (Jaysyn(..), fromString, toString)

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


toString : Jaysyn -> String
toString value =
    case value of
        Null ->
            "null"

        Bool True ->
            "true"

        Bool False ->
            "false"

        Float float ->
            if float == 0.0 then
                "0.0"

            else
                String.fromFloat float

        Int int ->
            String.fromInt int

        String string_ ->
            "\"" ++ string_ ++ "\""

        Array array ->
            "[" ++ String.join "," (List.map toString array) ++ "]"

        Object pairs ->
            "{"
                ++ String.join ","
                    (List.map
                        (\( key, value_ ) -> "\"" ++ key ++ "\":" ++ toString value_)
                        pairs
                    )
                ++ "}"


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
        , Parser.map String string

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


string : Parser String
string =
    Parser.succeed identity
        |. Parser.token "\""
        |= Parser.loop [] stringHelper


stringHelper : List String -> Parser (Parser.Step (List String) String)
stringHelper segmentsRev =
    Parser.oneOf
        [ Parser.succeed (\_ -> Parser.Loop ("\"" :: segmentsRev))
            |= Parser.token "\\\""
        , Parser.succeed (\_ -> Parser.Done (String.concat (List.reverse segmentsRev)))
            |= Parser.token "\""
        , Parser.succeed (\segment -> Parser.Loop (segment :: segmentsRev))
            |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\\' && c /= '"'))
        ]


objectItem : Parser ( String, Jaysyn )
objectItem =
    Parser.succeed Tuple.pair
        |= string
        |. Parser.spaces
        |. Parser.token ":"
        |. Parser.spaces
        |= Parser.lazy (\_ -> parser)
