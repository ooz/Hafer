module Hafer.Parser.Language
( diagramDef
) where

import Text.Parsec
import Text.Parsec.Token
import qualified Text.Parsec.Language as L

diagramDef = LanguageDef
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = "#"
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_'"
               , opStart        = opLetter L.emptyDef
               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }
