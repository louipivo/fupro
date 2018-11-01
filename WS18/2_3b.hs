f :: Int -> Int ->Int
f x y
  | y > 50 && x == 0 = y*2
  | y <= 50 && x == 0 = y+2
  | y == 0 && x < 100 = x*2
  | otherwise = x + y
--f :: Int -> Int
--f n
--  | n < 10 = 2 * n
--  | otherwise = n + 30
