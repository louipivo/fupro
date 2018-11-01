--Aufgabe2.2
div' :: Int -> Int -> Maybe Int
div' x y | y == 0 =  Nothing | True = Just (div x y)

--Aufgabe2.3
collatz :: Int -> Int
collatz n = if even(n) then div n 2 else 3*n + 1
--collatz x | even (x) = (x`div`2) | False = ((3*x)+1)

f :: Int -> Int -> Int
f = \x -> \y -> case x of 
	0 -> if y > 50 then y * 2 else y + 2 
	 ; _ -> if x < 100 then x * 2 else x + y

t :: Int -> Int -> Int
t x y = if x == 0 && y>50 then y*2 else if x == 0 && y<= 50 then y+2 else if y == 0 && x < 100 then x*2 else x + y

q :: Int -> Int -> Int
q 0 y | y > 50  = y * 2
q 0 y | y <= 50 = y + 2
q x 0 | x < 100 = x * 2
q x y = x + y



fib :: Int -> Int
fib n = case n of
	0 -> 1
	1 -> 1
	n -> fib(n-1) + fib(n-2)

--Aufgabe2.1
data Kunde = Kunde
	{ vorname ::  String
	, name :: String
	, adresse :: String
}

data Konto = Konto
	{ kontostand :: Int
	, besitzer :: Kunde
	}

b = Kunde "Timo" "Sante" "Schwerte"
a = Konto 500 b

c = Kunde 
	{ vorname = "Paul"
	, name = "Russ"
	, adresse= "Hombruch"
	}
z = Kunde
u = Konto
	{ kontostand = 500
	, besitzer = c
	}
--Aufgabe2.4
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z