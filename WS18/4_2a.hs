catMaybes :: [Int] -> [Int]
catMaybes _xs =[]
catMaybes xs = x:foldr (*) 2 catMaybes xs
--double :: [Int] -> Int
--double = foldr (*) 2
