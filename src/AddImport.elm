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

                    newSymbol : Maybe (List String)
                    newSymbol =
                        Maybe.map (\s -> [ s ]) mabSymbol

                    newImport =
                        case Dict.get moduleName importsDict of
                            Just imp ->
                                { imp
                                    | symbols =
                                        Maybe.map2
                                            (\symbols symbol ->
                                                List.append symbols symbol
                                            )
                                            imp.symbols
                                            newSymbol
                                }

                            Nothing ->
                                { moduleName = moduleName
                                , symbols = newSymbol
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
