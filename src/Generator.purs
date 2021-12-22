module Generator where

import AST
import Data.Either (Either)
import Prelude (bind, discard, pure, ($), (*>), (<*), (<>), show, (-), (+))
import Data.List 

generator :: Program -> String 
generator (P x) = generate 0 x

generate :: Int -> List Statement -> String
generate i (x:xs) = (space i) <> case x of 
  Right -> "Right\n" <> generate i xs 
  Left -> "Left\n" <> generate i xs 
  Increment -> "Increment\n" <> generate i xs 
  Decrement -> "Decrement\n" <> generate i xs 
  Return -> "Return\n" <> generate i xs  
  Bracket a -> "Bracket Open\n" <> generate (i+2) a <> space i <> "Bracket Close\n" <> generate i xs 
generate i Nil = ""


space :: Int -> String 
space 0 = ""
space i = " " <> space (i-1)
