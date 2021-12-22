module Executor where 

import AST (Program(..), Statement(..))
import Data.List (List(..), length, reverse, (:))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Prelude 


type State = Tuple Int (List Int)

limit :: Int
limit = 15 --0 to 15, 16 memory records

executor :: Program -> String 
executor (P xs) = let mem = ((0 /\ initialMem limit):Nil) in
  let result = execute xs mem in
  show (length result) <> "\n" <> print result <> "\n" <> printASCII result

initialMem :: Int -> List Int
initialMem 0 = Nil
initialMem i = (0:initialMem (i-1))

execute :: List Statement -> List State -> List State 
execute (x:xs) ((i /\ m):ms) = execute xs (exec x ((i /\ m):ms))
execute _ m = m


exec :: Statement -> List State -> List State
exec s ((i /\ m):ms) = case s of 
  Right -> if (i+1) > limit then ((0 /\ m):ms) else (((i+1) /\ m):ms)
  Left -> if (i-1) < 0 then ((limit /\ m):ms) else (((i-1) /\ m):ms)
  Increment -> ((i /\ modify true m i):ms) 
  Decrement -> ((i /\ modify false m i):ms)
  Return -> ((i /\ m):((i /\ m):ms))
  Bracket (xs) -> looped xs ((i /\ m):ms)
exec _ _ = Nil

isZero :: State -> Boolean
isZero (i /\ m) = getVal i m == 0


looped :: List Statement -> List State -> List State
looped xs mem = case mem of
  ((i /\ m):_) -> if isZero (i /\ m) then mem 
    else looped xs (execute xs mem)
  Nil -> Nil

getVal :: Int -> List Int -> Int
getVal i (x:xs) = if i==0 then x else getVal (i-1) xs
getVal _ _ = 0

modify :: Boolean -> List Int -> Int -> List Int 
modify b (m:ms) i = 
  if i==0 
  then 
    if b 
    then ((m+1):ms) 
    else ((m-1):ms) 
  else (m:modify b ms (i-1))  
modify _ _ _ = Nil



print :: List State -> String
print ((i /\ m):ms) = print ms <> show i <> " [" <> printList m <> "]\n"
print Nil = "" 

printList :: List Int -> String
printList Nil = ""
printList (x:Nil) = show x
printList (x:xs) = show x <> "," <> printList xs


printASCII :: List State -> String
printASCII ((i /\ m):ms) = printASCII ms <> ascii (getVal i m)
printASCII Nil = ""


ascii :: Int -> String
ascii i = case i of
  32 -> " " 
  33 -> "!"
  34 -> "\""
  35 -> "#"
  36 -> "$"
  37 -> "%"
  38 -> "&"
  39 -> "'"
  40 -> "("
  41 -> ")"
  42 -> "*"
  43 -> "+"
  44 -> ","
  45 -> "-"
  46 -> "."
  47 -> "/"
  48 -> "0"
  49 -> "1"
  50 -> "2"
  51 -> "3"
  52 -> "4"
  53 -> "5"
  54 -> "6"
  55 -> "7"
  56 -> "8"
  57 -> "9"
  58 -> ":"
  59 -> ";"
  60 -> "<"
  61 -> "="
  62 -> ">"
  63 -> "?"
  64 -> "@"
  65 -> "A"
  66 -> "B"
  67 -> "C"
  68 -> "D"
  69 -> "E"
  70 -> "F"
  71 -> "G"
  72 -> "H"
  73 -> "I"
  74 -> "J"
  75 -> "K"
  76 -> "L"
  77 -> "M"
  78 -> "N"
  79 -> "O"
  80 -> "P"
  81 -> "Q"
  82 -> "R"
  83 -> "S"
  84 -> "T"
  85 -> "U"
  86 -> "V"
  87 -> "W"
  88 -> "X"
  89 -> "Y"
  90 -> "Z"
  91 -> "["
  92 -> "\\"
  93 -> "]"
  94 -> "^"
  95 -> "_"
  96 -> "`"
  97 -> "a"
  98 -> "b"
  99 -> "c"
  100 -> "d"
  101 -> "e"
  102 -> "f"
  103 -> "g"
  104 -> "h"
  105 -> "i"
  106 -> "j"
  107 -> "k"
  108 -> "l"
  109 -> "m"
  110 -> "n"
  111 -> "o"
  112 -> "p"
  113 -> "q"
  114 -> "r"
  115 -> "s"
  116 -> "t"
  117 -> "u"
  118 -> "v"
  119 -> "w"
  120 -> "x"
  121 -> "y"
  122 -> "z"
  123 -> "{"
  124 -> "|"
  125 -> "}"
  126 -> "~"
  0 -> ""
  _ -> "Foo"

iicsa :: String -> Int
iicsa i = case i of
  " " -> 32 
  "!" -> 33 
  "\""-> 34 
  "#"-> 35 
  "$"-> 36 
  "%"-> 37 
  "&"-> 38 
  "'"-> 39 
  "("-> 40 
  ")"-> 41 
  "*"-> 42 
  "+"-> 43 
  ","-> 44 
  "-"-> 45 
  "."-> 46 
  "/"-> 47 
  "0"-> 48 
  "1"-> 49 
  "2"-> 50 
  "3"-> 51 
  "4"-> 52 
  "5"-> 53 
  "6"-> 54 
  "7"-> 55 
  "8"-> 56 
  "9"-> 57 
  ":"-> 58 
  ";"-> 59 
  "<"-> 60 
  "="-> 61 
  ">"-> 62 
  "?"-> 63 
  "@"-> 64 
  "A"-> 65 
  "B"-> 66 
  "C"-> 67 
  "D"-> 68 
  "E"-> 69 
  "F"-> 70 
  "G"-> 71 
  "H"-> 72 
  "I"-> 73 
  "J"-> 74 
  "K"-> 75 
  "L"-> 76 
  "M"-> 77 
  "N"-> 78 
  "O"-> 79 
  "P"-> 80 
  "Q"-> 81 
  "R"-> 82 
  "S"-> 83 
  "T"-> 84 
  "U"-> 85 
  "V"-> 86 
  "W"-> 87 
  "X"-> 88 
  "Y"-> 89 
  "Z"-> 90 
  "["-> 91 
  "\""-> 92 
  "]"-> 93 
  "^"-> 94 
  "_"-> 95 
  "`"-> 96 
  "a"-> 97 
  "b"-> 98 
  "c"-> 99 
  "d" ->100
  "e" ->101
  "f" ->102
  "g" ->103
  "h" ->104
  "i" ->105
  "j" ->106
  "k" ->107
  "l" ->108
  "m" ->109
  "n" ->110
  "o" ->111
  "p" ->112
  "q" ->113
  "r" ->114
  "s" ->115
  "t" ->116
  "u" ->117
  "v" ->118
  "w" ->119
  "x" ->120
  "y" ->121
  "z" ->122
  "{" ->123
  "|" ->124
  "}" ->125
  "~" ->126
  _ -> 0