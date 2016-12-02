module Parser exposing (..)

import Module exposing (Module)
import Import exposing (Import)
import Combine exposing (Parser, (<*>), (*>), (<*), string, regex, maybe, between, sepBy, while)
import Regex


whitespace : Parser e String
whitespace =
    while ((==) ' ')


nonWhitespace : Parser e String
nonWhitespace =
    while ((/=) ' ')


notImport : Parser e String
notImport =
    regex "[^(import)]*"


moduleName : Parser e String
moduleName =
    notImport
        *> string "import"
        *> between whitespace whitespace nonWhitespace


alias : Parser e (Maybe String)
alias =
    maybe <|
        string "as"
            *> between whitespace whitespace nonWhitespace


symbols : Parser e (Maybe (List String))
symbols =
    maybe <|
        Combine.map (Regex.split Regex.All (Regex.regex ", ?")) <|
            whitespace
                *> string "exposing ("
                *> (regex ".*(?=\\)\\s*(\n|$))")
                <* regex "\\)\\s*(\n|$)"


import_ : Parser e Import
import_ =
    Combine.succeed Import
        <*> moduleName
        <*> alias
        <*> symbols


imports : Parser e (List Import)
imports =
    Combine.many import_


everythingBeforeImports : Parser e String
everythingBeforeImports =
    regex "[^]*(?=import)"


everythingElse : Parser e String
everythingElse =
    regex "[^]*"


module_ : Parser e Module
module_ =
    Combine.succeed Module
        <*> everythingBeforeImports
        <*> imports
        <*> everythingElse


parseModule : String -> Result String Module
parseModule inputSrc =
    Combine.parse module_ inputSrc
        |> Result.map (\( _, _, mod ) -> mod)
        |> Result.mapError toString
