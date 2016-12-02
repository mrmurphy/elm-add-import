module Import exposing (..)


type alias Import =
    { moduleName : String
    , alias : Maybe String
    , symbols : Maybe (List String)
    }


toString : Import -> String
toString import_ =
    let
        sortedSymbols =
            import_.symbols
                |> Maybe.map List.sort
    in
        "import "
            ++ import_.moduleName
            ++ " "
            ++ (Maybe.withDefault "" <|
                    Maybe.map (\alias_ -> "as " ++ alias_ ++ " ") import_.alias
               )
            ++ (Maybe.withDefault "" <|
                    Maybe.map (\symbols -> "exposing (" ++ (String.join ", " symbols) ++ ")")
                        sortedSymbols
               )
