module Blatt01 where

-- Aufgabe 1.1 a)

data Konto = Konto Kontostand Kunde
data Kontostand = Kontostand Int
data Kunde = Kunde String String String


-- Aufgabe 1.1 b)

betrag = Kontostand 80
kunde = Kunde "Homer" "Simpson" "Springfield"

bspKonto = Konto betrag kunde

-- Aufgabe 1.1 c)

bspKonto2 = Konto (Kontostand 80) (Kunde "Sponge" "Bob" "Bikini Bottom")

-- Aufgabe 1.2 a)

data Dreieck = Int Int Int

-- Aufgabe 1.2 b)
eib ::  Either Int Bool
eib = Left 10

-- Aufgabe 1.2 c)
data Color = Red | Magenta | Blue | Cyan | Green | Yellow

mc :: Maybe Color
mc = Just Green

-- Aufgabe 1.2 d)
tup :: (Bool,())
tup = (True,())


-- Aufgabe 1.3 a)

Left 1 :: Either  Int b Just Int :: Maybe Int
-----------------------------------------------
(Left1,Just Int)::(Either Int b,Maybe Int)
-----------------------------------------------
x::Int , (Left 1,Just x)::t'
-----------------------------------------------
(\x -> (Left 1, Just x )) 1


--Aufgabe 1.3 b)
{-
-- Lösung hier einfügen.
-}
