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
        ]
