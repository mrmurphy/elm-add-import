module Parser exposing (..)

import Module exposing (Module)
import Import exposing (Import)
import Combine exposing (Parser, (<*>), (*>), (<*), string, regex, maybe, between, sepBy, while, or)
import Regex


whitespace : Parser e String
whitespace =
    while ((==) ' ')


eol : Parser e String
eol =
    regex "(\n|$)"


nonWhitespace : Parser e String
nonWhitespace =
    while ((/=) ' ')


anythingFollowedByNewlineWhitespaceOrEOL : Parser e String
anythingFollowedByNewlineWhitespaceOrEOL =
    regex ".*?(?=\n|\\s|$)"


moduleName : Parser e String
moduleName =
    string "import"
        *> whitespace
        *> anythingFollowedByNewlineWhitespaceOrEOL
        <* regex "(\\s*|\n|$)"


alias : Parser e (Maybe String)
alias =
    maybe <|
        string "as"
            *> whitespace
            *> anythingFollowedByNewlineWhitespaceOrEOL
            <* regex "(\\s*|\n|$)"


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
    regex "[^]*?(?=import)"


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
