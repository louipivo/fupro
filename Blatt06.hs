module Blatt06 where

-- Vorgegebene Datentypen dürfen nicht geändert werden.
data Nat = Zero | Succ Nat deriving Show
data PosNat = One | Succ' PosNat deriving Show
data Int' = Zero' | Plus PosNat | Minus PosNat deriving Show
data Colist a = Colist {split :: Maybe (a,Colist a)}
data Stream a = (:<) {hd :: a, tl :: Stream a}
infixr 5 :<
-- Angepasste Ausgabefunktionen für bessere Test- und Beispielausgaben.
instance Show a => Show (Colist a) where
  show (Colist Nothing) = "[]"
  show s = "Colist [" ++ tail (show' s) where
    show' s = case split s of
      Just (a,as) -> ',' : (show a ++ show' as)
      Nothing     -> "]"
instance Show a => Show (Stream a) where
  show s = "Stream [" ++ show' 8 s ++ "...]" where
    show' 0 _ = ""
    show' n (a:<as) = show a ++ "," ++ show' (n-1) as
-- Beispiele
nil :: Colist a
nil = Colist Nothing
co123 :: Colist Int
co123 = Colist (Just (1,Colist (Just (2,Colist (Just (3,Colist Nothing))))))
blink :: Stream Int
blink = 0:<1:<blink
nats :: Stream Int
nats = nats' 0 where
  nats' n = n :< nats' (n+1)

-- Aufgabe 6.1 a)
drei :: Nat
drei = Succ(Succ(Succ Zero))

-- Aufgabe 6.1 b)
zwei :: PosNat
zwei = Succ' One

-- Aufgabe 6.1 c)
mzwei :: Int'
mzwei = Minus(Succ' One)

-- Aufgabe 6.1 d)
data Rat = Rat Int' PosNat deriving Show

-- Aufgabe 6.1 e)
c :: Rat
c = Rat (Minus (One)) (Succ'(Succ'(One)))

-- Aufgabe 6.1 f)
c' :: Rat
c' = Rat (Minus (Succ' One)) (One)

-- Aufgabe 6.2 a)
natTake :: Nat -> [a] -> [a]
natTake (Succ n) (a:s) = a:natTake n s
natTake Zero s = []
natTake _ [] = []

-- Aufgabe 6.2 b)
natHoch :: (a -> a) -> Nat -> a -> a
f `natHoch` Zero = id
f `natHoch` (Succ n) = f `natHoch` n

-- Aufgabe 6.2 c)
colistConc :: Colist a -> Colist a -> Colist a
colistConc (Colist(Just(a,s))) (Colist(Just(a',s'))) = Colist(Just(a,colistConc s (Colist(Just(a',s'))) ))
colistConc _ nil = nil

-- Aufgabe 6.2 d)
colistReverse :: Colist a -> Colist a
colistReverse = undefined

-- Aufgabe 6.2 e)
stTakeWhile :: (a -> Bool) -> Stream a -> [a]
stTakeWhile f (a :< s) = if f a then a : stTakeWhile f s else []

-- Aufgabe 6.2 f)
stZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
stZipWith f (a :< s) (b :< s') =  (f a b :< stZipWith f s s')
