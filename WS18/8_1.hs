data Bintree a = Empty | Fork a (Bintree a) (Bintree a)

preorderB :: Bintree a -> [a]
preorderB (Fork a l r) = a : preorderB l ++ preorderB r
preorderB Empty = []


or_ :: Bintree Bool -> Bool
or_ (Fork a l r) =
   if a==True then True
   else False
