--Übungsblatt03 ; Thomas Alessandro Buse ; 192959 ; Gruppe: 17
--Aufgabe 3.1 a
power :: Int -> Int -> Int
power base expo = loop 1 base expo 
loop state base expo = if expo > 0 then loop (state*base) base (expo-1) else state

--Aufgabe 3.1 b
summe :: [Int] -> Int
summe ls = loopa 0 ls 
loopa state ls = if length ls > 0 then loopa (state + head ls) (tail ls) else state 

--Aufgabe 3.2 a
take 2 $ tail [2,3,5,4,1]
~> take 2 ( tail [2,3,5,4,1] )
~> take 2 [3,5,4,1]
~> [3,5]

--Aufgabe 3.2 b
head $ drop 2 [1,4,5,3,2]
~> head ( drop 2 [1,4,5,3,2] )
~> head [5,3,2]
~> 5

--Aufgabe 3.2 c
foldl (-) 8 [5, 2]
~> (foldl (-) 8 [5]) - 2
~> ((foldl (-) 8 []) - 5) - 2
~> (8 - 5) -2
~> 1

--Aufgabe 3.2 d
foldr (-) 8 [5, 2]
~> 5 - (foldr (-) 8 [2])
~> 5 - (2 - (foldr (-) 8 []))
~> 5 - (2 - 8)
~> 11


--Aufgabe 3.3 a
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) =  Just a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:s) = Just s

--Aufgabe 3.3 b
listEven :: [a] -> Bool
listEven [] = True
listEven [a] = False
listEven (_:s) = case listEven s of
	False -> True
	True -> False

