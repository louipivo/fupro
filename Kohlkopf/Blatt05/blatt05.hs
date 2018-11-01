--Übungsblatt05 ; Thomas Alessandro Buse ; 192959 ; Gruppe: 17
--Paul Rüssmann ; 196683

--Aufgabe 5.1

solutions :: [(Int,Int,Int)]
solutions = [(x,y,z) | x <- [..] , y <- [0..] , z <-[0..], 2*x^3 + 5*y + 2 == z^2]

--Aufgabe 5.2 a

data Nat = Zero | Succ Nat
data PosNat = One | Succ PosNat
data Int' = Zero' | Plus PosNat | Minus PosNat

data drei = Plus Three

--Aufgabe 5.2 b

data RAT = Zero | Int' PostNat --Zähler / Nenner 

--Aufgabe 5.2 c

data c = Minus Three Two

--Aufgbae 5.3 a

natLength :: [a] -> Nat
natLength (_:s) = Succ $ natLength s
natLength _ = Zero

--Aufgabe 5.3 b

natDrop :: Nat -> [a] -> [a]
natDrop Zero s = s
natDrop n (_:s) | n > Zero = natDrop (Pred n) s
natDrop _ [] = []

--Aufgabe 5.3 c

data Colist a = Colist fsplit :: Maybe (a,Colist a)

colistIndex :: Colist a -> Int -> a 
colistIndex Just (a,_) 0 = a
colistIndex Just(_,Colist a)  n | n > 0 = colistIndex a (n-1)

--Aufgabe 5.3 d

streamTake :: Int -> Stream a -> [a]
streamTake n ~(Cons x xs)
  | n == 0    = []
  | n > 0     =  x : (streamTake (n - 1) xs)


--Aufgabe 5.4 

type ID = Int
data Bank = Bank [(ID,Account)] deriving Show
data Account = Account { balance :: Int, owner :: Client } deriving Show
data Client = Client
{ name :: String
, surname :: String
, address :: String
} deriving Show

--Aufgabe 5.4 a

--credit :: Int -> ID -> Bank -> Bank
--credit b i k = 

--










