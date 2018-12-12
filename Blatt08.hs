module Blatt08 where

import Coalg (Bintree(..),leaf,btree1,btree4)

-- Vorgegebene Datentypen dürfen nicht geändert werden.
data Nat = Zero | Succ Nat
data PosNat = One | Succ' PosNat
data Int' = Zero' | Plus PosNat | Minus PosNat
data Edge = Links | Rechts deriving Show
type Node = [Edge]

-- Aufgabe 8.1 a)
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
instance Enum Int' where
  toEnum  0 = Zero'
  toEnum n | n > 0 = (Plus (toEnum1 (n)))
           | n < 0 = (Minus (toEnum1 (-n))) where
    toEnum1 :: Int -> PosNat
    toEnum1 1 = One
    toEnum1 n = Succ' (toEnum1(n-1))
  fromEnum Zero' = 0
  fromEnum (Plus n) = fromEnum1 n
  fromEnum (Minus n) = - (fromEnum1 n)

fromEnum1 :: PosNat -> Int
fromEnum1 One = 1
fromEnum1 (Succ' n) = fromEnum1 n + 1

-- Aufgabe 8.2 b)
instance Num Int' where
  fromInteger = toEnum . fromInteger
  negate (Plus a) = Minus a
  negate (Minus a) = Plus a
  signum Zero' = Zero'
  abs n = n
  (+) n m = toEnum(fromEnum(n) + fromEnum(m))
  (*) n m = toEnum(fromEnum(n) * fromEnum(m))

instance Eq Int' where
  (==) n m = fromEnum(n) == fromEnum(m)

instance Ord Int' where
  (<=) n m = fromEnum(n) <= fromEnum(m)

instance Show Int' where
  show = show . fromEnum

-- Aufgabe 8.2 c)
-- Mögliche Lösung von Übungsblatt 5.
-- Kann durch die eigene Lösung ersetzt werden.
-- Ändern Sie den Typ in [(Int',Int',Int')].
solutions :: [(Int',Int',Int')]
solutions = [ (toEnum(x), toEnum(y), toEnum(z)) | z <- [0..] , x <- [0..z] , y <- [0..z]
            , 5*x + 3*y^2 + 10 == z ]

-- Aufgabe 8.3 a)
sizeBintree :: Bintree a -> Int
sizeBintree (Fork a b c) = (sizeBintree b) + (sizeBintree c) + 1
sizeBintree (Empty) = 0

-- Aufgabe 8.3 b)
zipBintree :: Bintree a -> Bintree b -> Bintree (a,b)
zipBintree _ Empty = Empty
zipBintree Empty _ = Empty
zipBintree (Fork a1 l1 r1) (Fork a2 l2 r2) = Fork(a1, a2) (zipBintree l1 l2) (zipBintree r1 r2)

-- Aufgabe 8.3 c)
getSubbintree :: Bintree a -> Node -> Maybe (Bintree a)
getSubbintree = undefined
