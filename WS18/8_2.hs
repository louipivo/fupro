foldPosNat :: val -> (val -> val) -> PosNat -> val
foldNat val _ One = val
foldPosNat val f (Succ n) = f(foldPosNat val f n)

toInt :: PosNat -> Int
toInt = foldPosNat 1 (+1)
