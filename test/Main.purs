module Test.Main where

import Prelude (Unit, show, ($), (+), (-), (<>), (==)) 

import Effect (Effect)
import Effect.Class.Console (log)
import Data.List (List(..), (:))
import Data.Either (Either(..))
import Text.Parsing.Parser.Pos (Position(..))
import Parser (parser)
import Generator (generator)
import Executor (executor)
import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition)


main :: Effect Unit
main = do
  --log $ print $ reverse $ fibonacci 20 (1:(1:Nil))
  printAST <> execProg 

prog :: String 
prog = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

input :: String
input = "Hi"

execProg = log $ (case parser $ prog of 
    Left err -> let message = parseErrorMessage err in
      let pos = showPosition $ parseErrorPosition err in
      "Error: " <> message <> " at " <> pos
    Right p -> executor p)


printAST = log $ (case parser $ prog of 
    Left err -> let message = parseErrorMessage err in
      let pos = showPosition $ parseErrorPosition err in
      "Error: " <> message <> " at " <> pos
    Right p -> generator p)


showPosition :: Position -> String
showPosition (Position pos) = "line " <> show pos.line <> " column " <> show pos.column



fibonacci :: Int -> List Int -> List Int
fibonacci i list = if 0 == i then list else case list of
  (a:(b:_)) -> fibonacci (i-1) ((a+b):list)
  _ -> Nil

print :: List Int -> String
print (a:as) = show a <> "," <> print as
print _ = ""




