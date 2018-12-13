module Blatt09_4 where

import Expr
  ( Exp(..)
  , Store
  , Arith(..)
  , foldArith
  , StackCom(..)
  , Estate
  , codeAlg
  , execute
  , executeCom
  )

expr :: Exp String
expr = Sum [5 :* Var "x", 3 :* (Var "y" :^ 2), Con 10]

vars :: Store String
vars "x" = 7
vars "y" = 5

run :: [Int]
run = fst $ execute (foldArith codeAlg expr) ([],vars)

{- Füllen Sie die Tabelle aus.
Kommando | Stapel
---------|-----------
Push 5   | [5]
Load "x" | [5,7]
Mul 2    | [35]
Push 3   | [3,35]
Load "y" | [5,3,35]
Push 2   | [2,5,3,35]
Up       | [25,3,35]
Mul 2    | [75,35]
Push 10  | [10,75,35]
Add 3    | [120]
-}
