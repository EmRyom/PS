module Parser where


import AST (Program(..), Statement(..))
import Data.List (many)
import Control.Lazy (fix)
import Lexer (token)

import Data.Either (Either)

import Text.Parsing.Parser.Combinators (choice, option, optionMaybe, sepBy, try, (<?>))
import Prelude (bind, discard, pure, ($), (*>), (<*))
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.String (eof)
import Data.List 

type SParser a = Parser String a


statement :: SParser Statement
statement = fix allStat
  where 
    allStat _ = choice
      [ (do 
        token.reserved "+"
        pure Increment)
      , (do 
        token.reserved "-"
        pure Decrement)
      , (do 
        token.reserved "<"
        pure Left)
      , (do 
        token.reserved ">"
        pure Right)
      , (do 
        token.reserved "."
        pure Return)
      , token.brackets (do 
        a <- many $ try statement
        pure $ Bracket a)
      ] <?> "a statement"

program :: SParser Program
program = do
  a <- many $ try statement
  pure $ P a 

parser :: String -> Either ParseError Program 
parser input = runParser input (token.whiteSpace *> program <* eof)

