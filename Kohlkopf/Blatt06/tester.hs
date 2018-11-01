--Übungsblatt05 ; Thomas Alessandro Buse ; 192959 ; Gruppe: 17
--				  Paul Rüssmann ; 196683 
import Expr

--Aufgabe 6.1 a

--2*x^3 + 5*y + 2 :  
a :: Exp String
a = Sum [2:*(Var"x":^3),5:*(Var"y"),Con 2]
--z^2 :
b :: Exp String
b = (Var"z":^2)

--Aufgabe 6.1 b // Guck dir das mal an

--solutions :: [(Int,Int,Int)]
--solutions = [ (x,y,z)| z <- [0..], y <- [0..z^2], x <- [0..z^2], 2*x^3 + 5*y + 2 == z^2]
--f "x" = 2
--exp2store (Sum [Var"x",Con1])
f"z" =2
--solutions = exp2store b $ \"z" -> head [0..]
--solutions = exp2store (Sum[Var"x",Con 1],f)

p3 :: Exp String
p3 = Sum [Var"x", Con 1]

test = exp2store p3 $ \"x" -> 2

exp122 :: Exp String
exp122 = Sum [Var"x":^4, 5:*(Var"x":^3), 11:*(Var"x":^2), Con 222]

--Aufgabe 6.2 // ?
type BStore x = x -> Bool
{-
bexp2store :: BExp x -> Store x -> BStore x -> Bool
bexp2store be st bst = case be of 
						True_ _ _-> True
						False_ _ _-> False
						BVar x -> st x
						Or [BExp x] x	-> map (!!) x
						And [BExp x] 	-> map (&&) x
						Not (BExp x) 	-> (not) x
						be :< be' 	-> if be < be' then True; else False_
						be := be' 	-> if be == be' then True; else False
						be :<= be' 	-> if be > be' then True; else False 
				where eval = flip bexp2store st bs			
-}



{-
data BExp x = True_ | False_ | BVar x | Or [BExp x] | And [BExp x] |
              Not (BExp x) | Exp x :< Exp x | Exp x := Exp x | Exp x :<= Exp x
              deriving (Eq,Show)
-}
{-
data Exp x = Con Int | Var x | Sum [Exp x] | Prod [Exp x] |
Exp x :- Exp x | Int :* Exp x | Exp x :^ Int

exp2store :: Exp x -> Store x -> Int	   	-- exp2store = foldExp storeAlg
exp2store e st = case e of Con i   -> i
	                   Var x   -> st x
	                   Sum es  -> sum $ map eval es
	                   Prod es -> product $ map eval es
	                   e :- e' -> eval e - eval e'
	                   e :/ e' -> eval e `div` eval e'
	                   i :* e  -> i * eval e
	                   e :^ i  -> eval e ^ i
	         where eval = flip exp2store st	
	         -}



--Aufgabe 6.3

--data Colist a = Colist { split :: Maybe (a, Colist a)}
data Stream a = (:<) { hd :: a, tl :: Stream a}

class A a where
	drop' :: Int -> a -> a

instance A [a] where 
	drop' 0 s = s
	drop' n (a:s) | n > 0 = drop' (n-1) s
	drop' _ [] = []

{-
instance A (Colist a) where
	drop' 0 (Colist (Just(a,_)) = a  
	drop' n (Colist (Just (_:s))) | n > 0 = drop' (n-1) s
	drop' _ [] = []
-}

instance A Stream a where
	drop' 0 s = s
	drop' n (a:<s) |n >0 = drop' (n-1) s
	drop' _ [] = []


--Aufgabe 6.4
data Bintree a = Empty | Fork a (Bintree a) (Bintree a) deriving Show
data Edge = Links | Rechts deriving Show
type Node = [Edge]

--Aufgabe 6.4 a

--value :: Node -> Bintree a -> Maybe a --// Guck dir das mal an

--Aufgabe 6.4 b

--search :: Eq a => a -> Bintree a -> Maybe Node --// Guck dir das mal an

--search v (Fork s l r) if v == show(a) then 
--search v (Fork s l r) = if v == fromJust(value [] (Fork s l r)) then Just ([Rechts] ++ [Links])
--                        else if show(search v l) == "Nothing" then search v r
--                        else Just [Links]

{-
search v (Fork s l r) = if v == fromJust(value [] (Fork s l r)) then Just []
						else if v == fromJust(value [Links] l) then Just [Links]
							else if v == fromJust(value [Rechts] r) then Just [Rechts] 
								else 
									-}
{-
search v (Fork s l r) = if v == fromJust(value [] (Fork s l r)) then Just []
						else if v == fromJust(value [Links] l) then search v l
							else if v == fromJust(value [Rechts] r) then search v r
								else Nothing
								-}

--search v (Fork s l r) = if v == fromJust(value [] (Fork s l r)) then Just []
--						else ((search v r ) ++ Just [Rechts])	s
