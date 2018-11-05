module Blatt03 where

-- Aufgabe 3.1 a)
mult :: Int -> Int -> Int
mult x y = loop 0 x y
loop state x y = if y > 0 then loop (state+x) x (y-1) else state

-- Aufgabe 3.1 b)
prod :: [Int] -> Int
prod ls = loop1 1  ls
loop1 state ls = if length ls > 0 then loop1 (state*head ls) (tail ls) else state

-- Aufgabe 3.2 a)
{-
dropWhile (==2) [5,2,8,2]
if (==2) 5 then (==2) [2,8,2] else s
~>if (5 == 2) then dropWhile (==2)  [2,8,2] else s
~>false then dropWhile (==2)  [2,8,2] else s
~>s
~> [5,2,8,2]
-}

-- Aufgabe 3.2 b)
{-
take 4 [3,2,4,8,4,5]!!1
~> a:(take (n-1) s)!!1
~> 3:(take (4-1) s) !!1
~> 3:2:(take (3-1) s) !!0
~> 2
-}

-- Aufgabe 3.2 c)
{-
updList [3,2,8,4] 2 9

updList s i a = take s++a:drop (i+1) s

take 2 s++a:drop (3) s
~> [3,2] s++a:drop (3) s
~> [3,2,9]:drop 3 s
~> [3,2,9]:
-}

-- Aufgabe 3.3 a)
safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div`y)

-- Aufgabe 3.3 b)
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (a:_) 0 = Just a
safeIndex (_:s) x | x > 0 = safeIndex s (x-1)
                  | x < 0 = Nothing
