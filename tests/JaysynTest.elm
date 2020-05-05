module JaysynTest exposing (..)

import Expect
import Jaysyn
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
        ]
