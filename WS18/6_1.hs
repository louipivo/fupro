expr :: Exp String
expr = Sum [2 :* (Var 'x' :^ 3), 5 :* Var 'y', Con 2]

solutions :: [(Int, Int, Int)]
solutions = [(x,y,z) | z <- [0..], x<- [0..z^2], y<-[0..z^2]
            , exp2store expr(st x y) == z
            ]
  where
    st x y 'x' = x
    st x y 'y' = y
