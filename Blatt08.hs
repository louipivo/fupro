module Blatt08 where

import Coalg (Bintree(..),leaf,btree1,btree4)

-- Vorgegebene Datentypen dürfen nicht geändert werden.
data Nat = Zero | Succ Nat
data PosNat = One | Succ' PosNat
data Int' = Zero' | Plus PosNat | Minus PosNat
data Edge = Links | Rechts deriving Show
type Node = [Edge]

-- Aufgabe 8.1 a)
{-drop 0 s = s
drop n (_:s) | n > 0 = drop (n-1) s
drop _ [] = [] -}

class GenDrop a where genDrop :: a -> [b] -> [b]
-- Aufgabe 8.1 b)
instance GenDrop (Int) where	
	genDrop 0 s = s
	genDrop n (_:s) | n > 0 = genDrop (n-1) s
	genDrop _ [] = []
instance GenDrop (Nat) where
	genDrop Zero s = s
	genDrop (Succ n) (_:s) = genDrop n s
	genDrop _ [] = []
instance GenDrop (PosNat) where
	genDrop One (_:s) = s
	genDrop (Succ' n) (_:s) = genDrop n s
	genDrop _ [] = []
instance GenDrop (Int') where
	genDrop Zero' s = s
	genDrop (Plus (Succ' n)) (_:s) = genDrop n s
	genDrop (Minus (Succ' n))(_:s) = genDrop n s
	genDrop _ [] = []

-- Aufgabe 8.2 a)
{-instance Enum Int' where
  toEnum  0 = Zero'
  toEnum n | n > 0 = (Plus (Succ'(toEnum (n-1))))
           | n < 0 = (Minus (Succ'(toEnum (n+1))))
  --fromEnum Zero' = 0
  fromEnum (Plus (Succ' n)) = (fromEnum n) + 1
  fromEnum (Minus (Succ' n)) = (fromEnum n) - 1

-- Aufgabe 8.2 b)
instance Num Int' where
  fromInteger = toEnum . fromInteger
  negate (Plus a) = Minus a
  negate (Minus a) = Plus a
  signum Zero' = Zero'
  abs n = n
  (+) n m = toEnum(fromEnum(n) + fromEnum(m))
  --(+) Zero' n = n
  {-(Succ' n) (+) m = Succ' (n + m)
  Zero' (+) n = n
  (Succ' n) (*) m = m + (n*m)
  Zero' (*) n = Zero'
-}
instance Eq Int' where
  -- Zero' (==) Zero' = True
  -- Succ' n (==) Succ' m = n == m
  -- _ (==) _ = False

instance Ord Int' where
  -- Succ' n (<=) Succ' m = n <= m
  -- Zero' (<=) _ = True
  -- _ (<=) _ = False

instance Show Int' where
  show = show . fromEnum

-- Aufgabe 8.2 c)
-- Mögliche Lösung von Übungsblatt 5.
-- Kann durch die eigene Lösung ersetzt werden.
-- Ändern Sie den Typ in [(Int',Int',Int')].
solutions :: [(Int',Int',Int')]
solutions = [ (x,y,z) | z <- [0..] , x <- [0..z] , y <- [0..z]
            , 5*x + 3*y^2 + 10 == z ]

-}
-- Aufgabe 8.3 a)
{-data Bintree a = Empty | Fork a (Bintree a) (Bintree a)
leaf :: a -> Bintree a
leaf a = Fork a Empty Empty-}
sizeBintree :: Bintree a -> Int
sizeBintree (Fork a b c) = (sizeBintree b) + (sizeBintree c) + 1
sizeBintree (Empty) = 0


-- Aufgabe 8.3 b)
zipBintree :: Bintree a -> Bintree b -> Bintree (a,b)
zipBintree = undefined -- Durch Lösung ersetzen.

-- Aufgabe 8.3 c)
getSubbintree :: Bintree a -> Node -> Maybe (Bintree a)
getSubbintree = undefined -- Durch Lösung ersetzen.
