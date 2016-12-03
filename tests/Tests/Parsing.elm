module Tests.Parsing exposing (..)

import Test exposing (..)
import Expect
import Parser
import Module exposing (Module)
import Combine


fixture1 : String
fixture1 =
    """
module Foo exposing (..)

import B
import BB as BB
import A as A exposing (a, b, c)

""" ++ fixture1After


fixture1After : String
fixture1After =
    """more code, and I'm going to use the word
import here later in the file to test a
regression with the regex.
"""


expected1 : Module
expected1 =
    let
        imports =
            [ { moduleName = "B"
              , symbols = Nothing
              , alias = Nothing
              }
            , { moduleName = "BB"
              , symbols = Nothing
              , alias = Just "BB"
              }
            , { moduleName = "A"
              , symbols = Just [ "a", "b", "c" ]
              , alias = Just "A"
              }
            ]
    in
        { before = "\nmodule Foo exposing (..)\n\n"
        , imports = imports
        , after = fixture1After
        }


all : Test
all =
    describe "Parsing existing modules"
        [ describe "basic tests"
            [ test "Basic parsing" <|
                \() ->
                    case Parser.parseModule fixture1 of
                        Ok mod ->
                            Expect.equal mod expected1

                        Err e ->
                            Expect.fail <| toString e
            , test "Just symbols" <|
                \() ->
                    case Combine.parse Parser.symbols "exposing (a, b, c)" of
                        Ok ( _, _, res ) ->
                            Expect.equal res <| Just [ "a", "b", "c" ]

                        Err e ->
                            Expect.fail <| toString e
            , test "Just import" <|
                \() ->
                    case Combine.parse Parser.import_ "import A as AA exposing (a, b, c)" of
                        Ok ( _, _, res ) ->
                            Expect.equal res <|
                                { moduleName = "A"
                                , alias = Just "AA"
                                , symbols = Just [ "a", "b", "c" ]
                                }

                        Err e ->
                            Expect.fail <| toString e
            , test "multiple imports" <|
                \() ->
                    case Combine.parse Parser.imports "import A exposing (a, b, c)\nimport B exposing (b)" of
                        Ok ( _, _, res ) ->
                            Expect.equal res <|
                                [ { moduleName = "A"
                                  , alias = Nothing
                                  , symbols = Just [ "a", "b", "c" ]
                                  }
                                , { moduleName = "B"
                                  , alias = Nothing
                                  , symbols = Just [ "b" ]
                                  }
                                ]

                        Err e ->
                            Expect.fail <| toString e
            ]
        ]
