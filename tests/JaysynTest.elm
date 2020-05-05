module JaysynTest exposing (..)

import Expect
import Jaysyn
import Test exposing (..)


jaysynTest : Test
jaysynTest =
    describe "Jaysyn"
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
        ]
