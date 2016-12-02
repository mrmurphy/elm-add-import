module AddImport exposing (addImport)

{-| Add an import to an Elm module.

Write an example here:

``` elm
```

@docs addImport
-}

import Parser
import Module
import Import exposing (Import)
import Dict


{-| -}
addImport : String -> Maybe String -> String -> Result String String
addImport moduleName mabSymbol inputSrc =
    Parser.parseModule inputSrc
        |> Result.map
            (\module_ ->
                let
                    importsByName : List ( String, Import )
                    importsByName =
                        List.map (\({ moduleName } as imp) -> ( moduleName, imp )) module_.imports

                    importsDict =
                        Dict.fromList importsByName

                    newSymbol : List String
                    newSymbol =
                        case mabSymbol of
                            Nothing ->
                                []

                            Just s ->
                                [ s ]

                    newImport =
                        case Dict.get moduleName importsDict of
                            Just imp ->
                                { imp
                                    | symbols = Maybe.map (\symbols -> List.append symbols newSymbol) imp.symbols
                                }

                            Nothing ->
                                { moduleName = moduleName
                                , symbols = Just newSymbol
                                , alias = Nothing
                                }

                    updatedDict =
                        Dict.insert moduleName newImport importsDict
                in
                    { module_
                        | imports = Dict.values <| updatedDict
                    }
                        |> Module.toString
            )
