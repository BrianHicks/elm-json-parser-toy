module JaysynTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Jaysyn exposing (Jaysyn)
import Test exposing (..)


jaysynTest : Test
jaysynTest =
    describe "Jaysyn"
        [ describe "fromString"
            [ test "null" <|
                \_ ->
                    Jaysyn.fromString "null"
                        |> Expect.equal (Ok Jaysyn.Null)
            , describe "booleans"
                [ test "true" <|
                    \_ ->
                        Jaysyn.fromString "true"
                            |> Expect.equal (Ok (Jaysyn.Bool True))
                , test "false" <|
                    \_ ->
                        Jaysyn.fromString "false"
                            |> Expect.equal (Ok (Jaysyn.Bool False))
                ]
            , describe "numbers"
                [ test "floats" <|
                    \_ ->
                        Jaysyn.fromString "3.1415926535"
                            |> Expect.equal (Ok (Jaysyn.Float 3.1415926535))
                , test "ints" <|
                    \_ ->
                        Jaysyn.fromString "3"
                            |> Expect.equal (Ok (Jaysyn.Int 3))
                , test "hex" <|
                    \_ ->
                        Jaysyn.fromString "0x1DEA"
                            |> Expect.equal (Ok (Jaysyn.Int 0x1DEA))
                , test "negative numbers" <|
                    \_ ->
                        Jaysyn.fromString "-3"
                            |> Expect.equal (Ok (Jaysyn.Int -3))
                ]
            , describe "string"
                [ test "basic string" <|
                    \_ ->
                        Jaysyn.fromString "\"Hello, STL Elm!\""
                            |> Expect.equal (Ok (Jaysyn.String "Hello, STL Elm!"))
                ]
            , describe "array"
                [ test "empty" <|
                    \_ ->
                        Jaysyn.fromString "[]"
                            |> Expect.equal (Ok (Jaysyn.Array []))
                , test "homogeneous" <|
                    \_ ->
                        Jaysyn.fromString "[1, null]"
                            |> Expect.equal (Ok (Jaysyn.Array [ Jaysyn.Int 1, Jaysyn.Null ]))
                ]
            , describe "object"
                [ test "empty" <|
                    \_ ->
                        Jaysyn.fromString "{}"
                            |> Expect.equal (Ok (Jaysyn.Object []))
                , test "single" <|
                    \_ ->
                        Jaysyn.fromString "{\"hello\": \"world\"}"
                            |> Expect.equal (Ok (Jaysyn.Object [ ( "hello", Jaysyn.String "world" ) ]))
                ]
            ]
        , describe "toString"
            [ fuzz jaysynFuzzer "encodes a value that can roundtrip to the original" <|
                \jaysyn ->
                    jaysyn
                        |> Jaysyn.toString
                        |> Jaysyn.fromString
                        |> Expect.equal (Ok jaysyn)
            ]
        ]


nullFuzzer : Fuzzer Jaysyn
nullFuzzer =
    Fuzz.constant Jaysyn.Null


boolFuzzer : Fuzzer Jaysyn
boolFuzzer =
    Fuzz.map Jaysyn.Bool Fuzz.bool


floatFuzzer : Fuzzer Jaysyn
floatFuzzer =
    Fuzz.map Jaysyn.Float Fuzz.float


intFuzzer : Fuzzer Jaysyn
intFuzzer =
    Fuzz.map Jaysyn.Int Fuzz.int


stringValueFuzzer : Fuzzer String
stringValueFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant "a"
        , Fuzz.constant " "
        ]
        |> Fuzz.list
        |> Fuzz.map String.concat


stringFuzzer : Fuzzer Jaysyn
stringFuzzer =
    Fuzz.map Jaysyn.String stringValueFuzzer


arrayFuzzer : Fuzzer Jaysyn
arrayFuzzer =
    Fuzz.map Jaysyn.Array (Fuzz.list (Fuzz.oneOf [ nullFuzzer, boolFuzzer, floatFuzzer, intFuzzer, stringFuzzer ]))


objectFuzzer : Fuzzer Jaysyn
objectFuzzer =
    Fuzz.map Jaysyn.Object
        (Fuzz.list
            (Fuzz.map2 Tuple.pair
                stringValueFuzzer
                (Fuzz.oneOf [ nullFuzzer, boolFuzzer, floatFuzzer, intFuzzer, stringFuzzer ])
            )
        )


jaysynFuzzer : Fuzzer Jaysyn
jaysynFuzzer =
    Fuzz.oneOf
        [ nullFuzzer
        , boolFuzzer
        , floatFuzzer
        , intFuzzer
        , stringFuzzer
        , arrayFuzzer
        , objectFuzzer
        ]
