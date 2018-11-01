--Übungsblatt05 ; Thomas Alessandro Buse ; 192959 ; Gruppe: 17
--Paul Rüssmann ; 196683 

--Aufgabe 5.1

solutions :: [(Int,Int,Int)]
solutions = [(x,y,z) | z <- [0..] , y <- [0..z] , x <-[0..y], 2*x^3 + 5*y + 2 == z^2]

{-
Aufgabe 5.2 a

drei = Succ' Succ' One

--Aufgabe 5.2 b

data RAT = (Int',PostNat)

--Aufgabe 5.2 c

c=(-3,2)
-}

--Aufgabe 5.3
data Nat = Zero | Succ Nat deriving (Show)

--Aufgabe 5.3 a
natLength :: [a] -> Nat
natLength [] = Zero
natLength (_:xs) = Succ (natLength xs)

--Aufgabe 5.3 b

natDrop :: Nat -> [a] -> [a]
natDrop Zero xs = xs
natDrop _ [] = []
natDrop (Succ a) (x:xs) = natDrop a (xs)

data Stream a = (:<) {hd :: a, tl :: Stream a} deriving Show

--Aufgabe 5.3 c
--data Colist a = (:<) {hd :: Colist, tl :: a} deriving Show

--colistIndex :: Colist a -> Int -> a 
--colistIndex _ 0 = a
--colistIndex (c :< cs) n | n > 0 = colistIndex (cs) (n-1)

--Aufgabe 5.3 d

streamTake :: Int -> Stream a -> [a]
streamTake 0 _ = []
streamTake b (c :< cs) = c:(streamTake (b-1) (cs))

--Aufgabe 5.4

type ID = Int
data Bank = Bank [(ID,Account)] deriving Show
data Account = Account { balance :: Int, owner :: Client } deriving Show
data Client = Client
	{ name :: String
	, surname :: String
	, address :: String
	} deriving Show


c1 = Client "Thomas" "Buse" "OH12"
a1 = Account 500 c1
b1 = Bank [(8,a1)] 
c2 = Client "Paul" "Rüssmann" "OH12"
a2 = Account 250 c2
--b1 = Bank [(9,a2)] 

--Aufgabe 5.4 a

credit :: Int -> ID -> Bank -> Bank
credit money id (Bank b) = Bank(map(lookCredit money id) b)

lookCredit :: Int -> ID -> (ID,Account) -> (ID,Account)
lookCredit money id1 (id2,acc) = if id1 == id2 then (id2,acc{balance = balance acc + money}) else (id2,acc)

--Aufgabe 5.4 b

debit :: Int -> ID -> Bank -> Bank
debit money id (Bank b) = Bank(map(lookDebit money id) b)

lookDebit :: Int -> ID -> (ID,Account) -> (ID,Account)
lookDebit money id1 (id2,acc) = if id1 == id2 then (id2,acc{balance = balance acc - money}) else (id2,acc)

--Aufgabe 5.4 c

transfer :: Int -> ID -> ID -> Bank -> Bank
transfer money id1 id2 b = credit money id2 (debit money id1 b)