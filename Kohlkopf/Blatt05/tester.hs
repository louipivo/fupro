--tester

solutions :: [(Int,Int,Int)]
solutions = [(x,y,z) | z <- [0..] , y <- [0..z-1] , x <-[0..y-1], 2*x^3 + 5*y + 2 == z^2]

type ID = Int
data Bank = Bank [(ID,Account)] deriving Show
data Account = Account { balance :: Int, owner :: Client } deriving Show
data Client = Client
	{ name :: String
	, surname :: String
	, address :: String
	} deriving Show



--Aufgabe 5.4 a

--lookup :: Eq a => a -> [(a,b)] -> Maybe b
--lookup a ((a,b):r) = if a == a then Just b else lookup a r
--lookup _ _ = Nothing

--look :: ID -> [(ID,Account)] -> Account
--look i ((q,a):r) = if q == q then a else look i r

--look :: ID -> Bank -> Account
--look i ((q,a):r) = if q == q then a else look i r

--test :: Int -> ID -> Account -> Int --Aufruf: test 5 8 a1 -> 500
--test b i k = balance k

--lookID :: ID -> Bank
--lookID i = if i == ID Bank then Account Bank else lookID i 

--test1 :: Int -> ID -> Bank -> Account
--test1 b i k = look i (i,k)

--credit :: Int -> ID -> Bank -> Bank
--credit b (ID i) (Bank k) = Bank $ map credis as where
--	credis::

--doubleSalaryA :: FirmaA -> FirmaA
--doubleSalaryA (FirmaA as) = FirmaA $ map doubleSalaryAbt as where

  --doubleSalaryAbt :: AbteilungA -> AbteilungA
  --doubleSalaryAbt ab = ab { mitarbeiterA = map doubleSalaryM (mitarbeiterA ab) } 

  --doubleSalaryM :: MitarbeiterA -> MitarbeiterA
  --doubleSalaryM m = m { mitarbeiterGehalt = 2 * mitarbeiterGehalt m }

  --lookID :: ID -> ID
  --lookID (ID i) = ID & map lookBank i where

  	--lookBank :: Bank -> Bank
  	--lookBank b = b { Account = map lookAccount (Account b)}

  	--lookAccount :: Account -> Account
  	--lookAccount a = a {balance = balance +1}

--tupel :: Bank -> Bank
--tupel (Bank(i,b)) = b

--fst' :: Bank -> ID
--fst' (Bank (x,_)) = x

--tupel1 :: (ID,Account) -> Account
--tupel1 (a,b) = b

hallo :: ID -> Bank
hallo = Bank [a,b] where
	a == id