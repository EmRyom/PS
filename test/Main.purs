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
  --log $ print $ fibonacci 20 (1:(1:Nil)) {-
  log $ case parser $ prog of 
    Left err -> let message = parseErrorMessage err in
      let pos = showPosition $ parseErrorPosition err in
      "Error: " <> message <> " at " <> pos
    Right p -> generator p <> executor p
  --}

prog :: String 
prog = --"++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+"  --Hello World!
       --"++[>++++++<-]>>++[>+++++++<-]>." --Numbers
       --"++++[>++++[>++++[>++++<-]<-]<-]>>>>." --4^4
       "+>+>>++++[<+++++>-] <[<[->>+>+<<<] >>[<<+>>-] <<[<+>-] >>>[<<<+>>>-].<<- <<[>>>+>+<<<<-] >>>[<<<+>>>-] <<<[>+<-] >>>>[<<<<+>>>>-].<<- ]"
    --  1 1  limit : 20     Copying 1     Add 1 back  Calc Fib Recall 1      |   Copy var       add var back  calc fib   recall var 
    --                                                                  Decrement limit
       --"+++++>++<[>>+>+<<<-].>>[<<+>>-]."

input :: String
input = "Hi"

showPosition :: Position -> String
showPosition (Position pos) = "line " <> show pos.line <> " column " <> show pos.column

fibonacci :: Int -> List Int -> List Int
fibonacci i list = if 0 == i then list else case list of
  (a:(b:_)) -> fibonacci (i-1) ((a+b):list)
  _ -> Nil

print :: List Int -> String
print (a:as) = print as <> "," <> show a
print _ = ""