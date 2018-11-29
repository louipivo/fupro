module Blatt04 where

import Data.Char (toUpper)
import Examples (getIndex,perms)

-- Aufgabe 4.1 a)
shift  :: Int -> [a] -> [a]
shift 0 s = s
shift n (a:s) | n > 0 = shift(n-1) (s ++ [a])
shift _ [] = []

-- Aufgabe 4.1 b)
removeLetterA :: String -> String
removeLetterA (a:s) = if  a == 'A' then removeLetterA s else a:removeLetterA s
removeLetterA _ = []

-- Aufgabe 4.2 a)
cap :: String -> String
cap = map toUpper

-- Aufgabe 4.2 b)
lesser :: [Int] -> [Int] -> [Int]
lesser = zipWith min

-- Aufgabe 4.2 c)
applyToOne :: [a -> b] -> a -> [b]
applyToOne as a = map ($ a) as

-- Aufgabe 4.3 a)
{-
foldl (/) 20 [5, 4]
~> foldl (/) ((/) 20 5) [4]
~> foldl (/) 4 [4]
~> foldl (/) ((/) 4 4) []
~> foldl (/) 1 []
~> 1
-}

-- Aufgabe 4.3 b)
{-
foldr (/) 20 [5, 4]
~> (/) 5 $ foldr (/) 20 [4]
~> (/) 5 $ (/) 4 $ foldr (/) 20 []
~> (/) 5 $ (/) 4 $ 20
~> (/) 5 ((/) 4 20)
~> (/) 5 0,2
~> 25
-}

-- Aufgabe 4.4 a)
countNothing :: [Maybe a] -> Int
countNothing (a:s) = foldl countN 0 (a:s) where
countN state (Just a) = state
countN state (Nothing)  = state+1

-- Aufgabe 4.4 b)
lefts :: [Either a b] -> [a]
lefts = undefined -- Durch Lösung ersetzen.

-- Aufgabe 4.5 a)
squares :: [(Int,Int)]
squares = [(a, a*a) | a <- [0..10], even a]

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
