module Blatt05 where

-- Aufgabe 5.1 a)
{-
take 2 $ nats 3
($) take 2 nats 3
($) take 2 (3:map(+1)(nats 3))
($) take 1 (4:map(+1)(nats 4))
[3,4]
-}

-- Aufgabe 5.1 b)
{-
iterate tail [3,4,9,8] !! 2
(!!) (iterate tail [3,4,9,8]) 2
(!!) ([3,4,9,8]:iterate tail [4,9,8]) 2
(!!) ([4,9,8]:iterate tail [9,8]) 1
(!!) ([9,8]:iterate tail[8]) 0
([9,8]
-}

-- Aufgabe 5.1 c)
{-
fibs !! 2
(!!) fibs 2
-}

-- Aufgabe 5.2 a)
odds :: [Int]
odds = 1: map(+2) odds

-- Aufgabe 5.2 b)
alternate :: [Int]
alternate = 1 : (negate 1) : alternate

-- Aufgabe 5.2 c)
solutions :: [(Int, Int, Int)]
solutions = [(x,y,z)
  | z <- [0..]
  , x <- [0..z]
  , y <- [0..z]
  , 5*x + 3*y^2 + 10 == z
  ]

-- Funktion von Folie 53.
updRel :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
updRel ((a,b):r) c d = if a == c then (a,d):r else (a,b):updRel r c d
updRel _ a b = [(a,b)]

-- Vorgabe
type ID = Int
type Bank = [(ID,Account)]
data Account = Account { balance :: Int, owner :: Client }
  deriving Show
data Client = Client
  { name :: String
  , surname :: String
  , address :: String
  } deriving Show

-- Aufgabe 5.3 a)
credit :: Int -> ID -> Bank -> Bank
credit amount id ls
  = updRel ls id entry { balance = oldBalance + amount}
  where
    Just entry = lookup id ls
    oldBalance = balance entry

-- Aufgabe 5.3 b)
debit :: Int -> ID -> Bank -> Bank
debit amount = credit (-amount)

-- Aufgabe 5.3 c)
transfer :: Int -> ID -> ID -> Bank -> Bank
transfer amount id1 id2 = debit amount id1 . credit amount id2
