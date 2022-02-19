module AST where

import Data.List

data Program = P (List Statement)

data Statement 
  = Right 
  | Left
  | Increment
  | Decrement
  | Return
  | Enter 
  | Bracket (List Statement)
