--Aufgabe 5.1
solutions :: [(Int, Int, Int)]
solutions = [(x,y,z) | z <- [0..], x <- [0..z^2], y <- [0..z^2], 2*x^3 + 5*y + 2 == z^2]

--Aufgabe 5.2
data Nat = Zero | Succ Nat
data PosNat = One | Succ' PosNat
data Int' = Zero' | Plus PosNat | Minus PosNat

--Aufgabe 5.2 a)
drei = Plus $ Succ' $ Succ' One

--Aufgabe 5.2 b)
data Rat = Rat Int' PosNat

--Aufgabe 5.2 c)
c = Rat (Minus $ Succ' $ Succ' One) (Succ' One)

--Aufgabe 5.3 a)
natLength :: [a] -> Nat
natLength [] = Zero
natLength (_:xs) = Succ (natLength xs)

--Aufgabe 5.3 b)
natDrop :: [a] -> [a]
natDrop Zero xs = xs
natDrop _ [] = []
natDrop (Succ a) (x:xs) = natDrop a (xs)

--Aufgabe 5.3 c)
data Colist a = Colist { split :: Maybe (a, Colist a)}

colistIndex :: Colist a -> Int -> a
colistIndex (Colist (Just (a,_))) 0 = a
colistIndex (Colist (Just (_:s))) n | n > 0 = colistIndex s (n-1)

--Aufgabe 5.3 d)
data Stream a = (:<) { hd :: a, tl :: Stream a}

streamTake :: Int -> Stream a -> [a]
streamTake 0 _ = []
streamTake n (a:<s) |n >0 = a:streamTake (n-1) s

--Aufgabe 5.4
type ID = Int
data Bank = Bank [(ID,Account)] deriving Show
data Account = Account { balance :: Int, owner :: Client } deriving Show
data Client = Client
    { name :: String
    , surname :: String
    , address :: String
    } deriving Show

--Aufgabe 5.4 a)
--Was abgegeben wurde:
-- credit :: Int -> ID -> Bank -> Bank
-- credit money id (Bank b) = Bank(map(lookCredit money id) b)

-- lookCredit :: Int -> ID -> (ID,Account) -> (ID,Account)
-- lookCredit money id1 (id2,acc) = if id1 == id2 then (id2, ecc {balance = balance acc + money})
--                                  else (id2, acc)
--Musterlösung:
credit :: Int -> ID -> Bank -> Bank
credit amount i (Bank bank) = Bank $ map f bank where
	f (i', Account b o)
	  | i' == i = (i, Account (amount + b) o)
	  | otherwise = (i', Account b o)

--Aufgabe 5.4 b)
--Was abgegeben wurde:
-- debit :: Int -> ID -> Bank -> Bank
-- debit money id (Bank b) = Bank(map(lookDebit money id) b)

-- lookDebit :: Int -> ID -> (ID,Account) -> (ID,Account)
-- lookDebit money id1 (id2, acc) = if id1 == id2 then (id2, acc{balance = balance acc - money})
-- 	                             else (id2, acc)
--Musterlösung:
debit :: Int -> ID -> Bank -> Bank
debit amount = credit (-amount)

--Aufgabe 5.4 c)
--Was abgegeben wurde:
-- transfer :: Int -> ID -> ID -> Bank -> Bank
-- transfer money id1 id2 b = credit money id2 (debit money id1 b)
--Musterlösung:
transfer :: Int -> ID -> ID -> Bank -> Bank
transfer amount src dst = (credit amount dst . debit amount src)


