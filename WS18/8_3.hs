expr :: Exp String
expr = Prod [Con 2, Con 9 :- Var "x"]

vars :: Store String
vars "x" = 6


Kommandos:      Stack:
Push 2          2
Push 9          9 2
Load 'x'        6 9 2
Sub 6           3 2
Prod 6          6
