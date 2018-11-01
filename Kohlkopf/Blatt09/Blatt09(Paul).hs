--Blatt09
{-# LANGUAGE TypeFamilies #-}
import Examples

--Aufgabe1a)
fibo 0 = 1
fibo 1= 1
fibo n = fibo(n-1) + fibo(n-2)

--b)

--Aufgabe 2a)
isCyclic :: Eq a => Graph a -> Bool
isCyclic g = loop $ graph2Rel $ closureF g
	where 
		loop [] = False 
		loop ((a,b):xs) = elem (b,a) xs || loop xs

--b)

--Aufgabe 3a
class C f where
	comp :: f b c -> f a b -> f a c
--(* -> * -> *)

--b)
data T f g = T (f String Int) (g Bool)
--T :: (* -> * -> *) -> (* -> *) -> *

--Aufgabe 4a)
class Listable l where
	type Item l :: *
	toList :: l -> [Item l]

data Colist a = Colist {split :: Maybe (a,Colist a)}
nil :: Colist a
nil = Colist Nothing

instance Listable (Colist a) where
	type Item (Colist a) = a
	toList (Colist Nothing) = []
	toList (Colist(Just(a,b))) = a : (toList b)
--b)
data Map a b = Map [(a,b)]
instance Listable (Map a b) where
	type Item (Map a b) = b
	toList (Map []) = []
	toList (Map ((a,b) : xs)) = b: toList (Map xs)
--c)
data Nat = Zero | Succ Nat
instance Listable Nat where
	type Item Nat = ()
	toList (Zero) = []
	toList (Succ a) = () : (toList a)



