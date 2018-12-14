module Blatt09_1 where

infixl 6  :-
infixl 7 :/, :*
data Exp x = Con Int | Var x | Sum [Exp x] | Prod [Exp x] |
             Exp x :- Exp x | Exp x :/ Exp x | Int :* Exp x |
             Exp x :^ Int

infix 4 :=, :<=
data BExp x = True_ | False_ | BVar x | Or [BExp x] |
              And [BExp x] | Not (BExp x) | Exp x := Exp x |
              Exp x :<= Exp x

bexpr1 :: BExp String
bexpr1 = Or [BVar "b", Not (BVar "b")]

bexpr2 :: BExp String
bexpr2 = And [Or [BVar "x", False_], BVar "y"]

bexpr3 :: BExp String
bexpr3 = And [BVar "b", Var "x" :<= Sum [Var "x", Con 10]]


instance Show x => Show (Exp x) where
  showsPrec _ (Con i) = shows i
  showsPrec _ (Var x) = shows x
  showsPrec p (Sum es) = showParen (p > 6) $ showMore 6 '+' es
  showsPrec p (Prod es) = showParen (p > 7) $ showMore 7 '*' es
  showsPrec p (e :- e') = showParen (p > 6)
    $ showsPrec 6 e . showChar '-' . showsPrec 7 e'
  showsPrec p (e :/ e') = showParen (p > 7)
    $ showsPrec 7 e . showChar '/' . showsPrec 8 e'
  showsPrec p (i :* e) = showParen (p > 7)
    $ shows i . showChar '*' . showsPrec 7 e
  showsPrec p (e :^ i) = showParen (p > 8)
    $ showsPrec 8 e . showChar '^' . shows i

enclose :: Bool -> (String -> String) -> String -> String
enclose = showParen

showMore :: Show a => Int -> Char -> [a] -> String -> String
showMore p op (e:es) = foldl f (showsPrec p e) es where
  f state e = state . showChar op . showsPrec p e

-- Aufgabe 9.1
instance Show x => Show (BExp x) where
  showsPrec _ (True_) = showString "true"
  showsPrec _ (False_) = showString "false"
  showsPrec _ (BVar x) = shows x
  showsPrec p (Or es) = showParen (p > 2) $ showMore 2 '|' es
  showsPrec p (And es) = showParen (p > 3) $ showMore 3 '&' es
  showsPrec p (Not e) = showChar '!' . showsPrec 10 e
  showsPrec p (e := e') = showParen (p > 4)
    $ showsPrec 6 e . showChar '=' . showsPrec 7 e'
  showsPrec p (e :<= e') = showParen (p > 4)
    $ showsPrec 6 e . showString "<=" . showsPrec 7 e'
