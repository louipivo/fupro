module Blatt04 where

import Data.Char (toUpper)
import Examples (getIndex,perms)

-- Aufgabe 4.1 a)
--shift :: ?? -- Typ hier einfügen.
--shift = undefined -- Durch Lösung ersetzen.

-- Aufgabe 4.1 b)
removeLetterA :: String -> String
removeLetterA = filter (`notElem` ['A'])



-- Aufgabe 4.2 a)
cap :: String -> String
cap = map toUpper

-- Aufgabe 4.2 b)
lesser :: [Int] -> [Int] -> [Int]
lesser = zipWith min

-- Aufgabe 4.2 c)
applyToOne :: [a -> b] -> a -> [b]
applyToOne = undefined -- Durch Lösung ersetzen.



-- Aufgabe 4.3 a)
{-
-- Lösung hier einfügen.
-}

-- Aufgabe 4.3 b)
{-
-- Lösung hier einfügen.
-}

-- Aufgabe 4.4 a)
countNothing :: [Maybe a] -> Int
countNothing = undefined -- Durch Lösung ersetzen.

-- Aufgabe 4.4 b)
lefts :: [Either a b] -> [a]
lefts = undefined -- Durch Lösung ersetzen.



-- Aufgabe 4.5 a)
squares :: [(Int,Int)]
squares = undefined -- Durch Lösung ersetzen.

-- Aufgabe 4.5 b)
solutions :: [(Int, Int, Int)]
solutions = [(x,y,z)
  | z <- [0..100]
  , y <- [0..100]
  , x <- [0..100]
  , 5*x + 3*y^2 + 10 == z
  ]

-- Aufgabe 4.5 c)
codes :: [[(Char,Int)]]
codes = [zip code [0..]
  | code <- perms "einsvrfu0"
  , let [e,i,n,s,v,r,f,u] = map (getIndex code) "einsvrfu"
  , 1000*(e+v) + 100*(i+i) + 10*(n+e) + (s+r)
  == 10*f + u
  ]
