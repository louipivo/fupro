--Übungsblatt04 ; Thomas Alessandro Buse ; 192959 ; Gruppe: 17
import Data.Maybe
{-# LANGUAGE LambdaCase #-}
--Aufgabe 4.1 a
double :: [Int] -> [Int]
double ls = map (*2) ls 

--Aufgabe 4.1 b
funs :: Int -> [Int]
funs a = map ($a) [(+1), (*2), (^2)]

--Aufgabe 4.1 c
toUnicode :: String -> [Int]
toUnicode a = map(fromEnum) a


--Aufgabe 4.2 a

f :: Maybe a -> [a] -> [a]
f (Just a) as = a:as
f Nothing as = as

--catMaybes :: [Maybe a] -> [a]
--catMaybes ls = foldl (fromJust ls) [] ls 
--catMaybes ls = foldl(\case Just a -> a; _ -> ) ls 

--Aufgabe 4.2 b
data Color = Red | Green | Blue
data Counter = Counter {red, green, blue :: Int} deriving Show

--count :: [Color] -> Counter
--count ls = red+1

--Aufgabe 4.3 a
divisors :: Int -> [Int]
divisors x = [y | y <- [1..x], x `mod` y == 0]


--Aufgabe 4.3 b
--codes :: [[(Char,Int)]]

codess ::[(Int,Int,Int,Int,Int,Int,Int,Int,Int)]
codess = [(z,w,e,i,v,r,s,c,h)|s <- [1..9], w <- [1..9], e <- [1..9], i <- [1..9], v <- [1..9], r <- [1..9], z <- [1..9], c <- [1..9], h <- [1..9], 1000*(z+v) + 100*(w+i) + 10*(e+e) + (i+r) == 10000*s + 1000*e + 100*c + 10*h +s]



--codes = [zipWithInds code | code <- perms "sendmory01",
--	let [s,e,n,d,m,o,r,y] = map (getIndex code) 1000*(s+m)+100*(e+o)+10*(n+r)+d+e == 10000*m+1000*o+100*n+10*e+y]

--getIndex :: Eq a => [a] -> a -> Int
--getIndex s a = fromJust $ lookup a $ zipWithInds s

--zipWithInds :: [a] -> [(a,Int)]
--zipWithInds s = zip s $ indices s

--indices :: [a] -> [Int]
--indices s = [0..length s-1]


--Aufgabe 4.3 c

solutions :: [(Int,Int,Int)]
solutions = [(x,y,z) | x <- [0..100], y <- [0..100], z <- [0..100], z^2 == 2*x^3 + 5*y +2]

--Aufgabe 4.4 a
--[x*2 | x <- [1..]]
--Aufgabe 4.4 b
 