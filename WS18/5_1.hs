solutions :: [(Int, Int, Int)]
solutions = [(x,y,z)
  | z <- [0..]
  , y <- [0..z]
  , x <- [0..z]
  , 3*x^2 + 2*y + 1 == z]
