module Module exposing (..)

import Import exposing (Import)


type alias Module =
    { before : String
    , imports : List Import
    , after : String
    }


toString : Module -> String
toString module_ =
    let
        sortedImports =
            List.sortBy (\imp -> imp.moduleName) module_.imports
    in
        module_.before
            ++ (List.map Import.toString sortedImports |> String.join "\n")
            ++ "\n\n"
            ++ module_.after
