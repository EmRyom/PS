module Lexer where

import Control.Alt ((<|>))
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser)

languageDef :: LanguageDef
languageDef = LanguageDef
  { commentStart: "(*"
  , commentEnd: "*)"
  , commentLine: ""
  , nestedComments: false
  , identStart: letter
  , identLetter: alphaNum <|> oneOf ['_', '\'']
  , opStart: oneOf ['+', '-', '#']
  , opLetter: oneOf ['+', '-', '#']
  , reservedNames: [ ]
  , reservedOpNames: ["+", "-", "<", ">", ".", ","]
  , caseSensitive: true
}

token :: TokenParser
token = makeTokenParser languageDef
