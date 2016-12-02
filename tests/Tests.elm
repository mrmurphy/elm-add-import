module Tests exposing (..)

import Test exposing (..)
import Expect
import AddImport
import Tests.Parsing


fixture1 : String
fixture1 =
    """
module Foo exposing (..)

import Bar as Blah exposing (A(..), b, c)

more code
"""


expected1 : String
expected1 =
    """
module Foo exposing (..)

import Added exposing (added)
import Bar as Blah exposing (A(..), b, c)

more code
"""


fixtureSorts : String
fixtureSorts =
    """
module Foo exposing (..)

import A exposing (c, b, a)

more code
"""


expectedSorts : String
expectedSorts =
    """
module Foo exposing (..)

import A exposing (a, b, c)
import Z exposing (z)

more code
"""


fixtureMerges : String
fixtureMerges =
    """
module Foo exposing (..)

import A exposing (b)

more code
"""


expectedMerges : String
expectedMerges =
    """
module Foo exposing (..)

import A exposing (a, b)

more code
"""


all : Test
all =
    describe "Adding imports"
        [ describe "basic tests"
            [ test "Adds an import" <|
                \() ->
                    Expect.equal
                        (AddImport.addImport "Added" (Just "added") fixture1 |> Result.withDefault "failed")
                        expected1
            , test "Sorts imports" <|
                \() ->
                    Expect.equal
                        (AddImport.addImport "Z" (Just "z") fixtureSorts |> Result.withDefault "failed")
                        expectedSorts
            , test "Merges new imports" <|
                \() ->
                    Expect.equal
                        (AddImport.addImport "A" (Just "a") fixtureMerges |> Result.withDefault "failed")
                        expectedMerges
            ]
          -- , Tests.Parsing.all
        ]
