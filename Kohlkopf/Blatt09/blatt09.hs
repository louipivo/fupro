--Übungsblatt09 ; Thomas Alessandro Buse ; 192959 ; Gruppe: 17
--				  Paul Rüssmann ; 196683 
{-# LANGUAGE TypeFamilies #-}
import Examples
--Aufgabe 9.1 a

--Aufgabe 9.1 b

--Aufgabe 9.2
--a)

isCyclic :: Eq a => Graph a -> Bool
isCyclic g = loop $ graph2Rel $ closureF g
	where 
		loop [] = False 
		loop ((a,b):xs) = elem (b,a) xs || loop xs

--b)
--depthFirst :: Eq a => a -> Graph a -> [a]
--depthFirst s (G nodes sucs) = [s] ++ depthFirst (head nodes) (G nodes sucs)

{-
depthFirst :: Eq a => a -> Graph a -> [a]
depthFirst start g = dfs [start] []
  where
    dfs [] visited = visited
    dfs (a:as) visited
       | elem a visited = dfs as visited
       | otherwise = dfs ((reachables g a)++as) (visited++[a])
-}

--Aufgabe 9.3
--a)
class C f where
	comp :: f b c -> f a b -> f a c
-- f :: (* -> * -> *)

--b)
data T f g = T (f String Int) (g Bool)
--T :: (* -> * -> *) -> (* -> *) -> *


--Aufgabe 9.4 
class Listable l where
	type Item l :: *
	toList :: l -> [Item l]	

--a)
data Colist a = Colist {split :: Maybe (a,Colist a)}

instance Listable (Colist a) where
	type Item (Colist a) = a
	toList (Colist Nothing) = []
	toList (Colist (Just(a,b))) = a : (toList b)

--b)
data Map a b = Map [(a,b)]

instance Listable (Map a b) where
	type Item (Map a b) = b
	toList (Map []) = []
	toList(Map ((a,b) : xs)) = b : toList (Map xs)

--c)
data Nat = Zero | Succ Nat

instance Listable Nat where
	type Item Nat = ()
	toList (Zero) = []
	toList (Succ a) = () : (toList a)

