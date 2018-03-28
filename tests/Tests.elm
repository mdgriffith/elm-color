module Example exposing (..)

{-| Mainly to test round tripping results through color spaces.

Need to wait for 0.19 before it would work.

-}

import Color
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


black =
    Color.rgb 0 0 0


white =
    Color.rgb 255 255 255


green =
    Color.rgb 0 255 0


suite : Test
suite =
    describe "Color Tests"
        [ test "" <|
            \_ ->
                Expect.equal black
                    black
        ]
