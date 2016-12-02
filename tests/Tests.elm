module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


all : Test
all =
    describe "Foo suite"
        [ describe "test group"
            [ test "A sample" <|
                \() ->
                    Expect.equal (3 + 7) 10
            ]
        ]
