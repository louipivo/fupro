-- Modulename und Importe dürfen nicht entfernt werden.
module Blatt10 where
import Examples
  ( Graph(..)
  , GraphL(..)
  , BinRel
  , BRfun
  , TRfun
  , Semiring(..)
  , graph1
  , graph2
  , graph3
  , graph4
  , graph2Rel
  , rel2Graph
  , closureF
  , closureT
  , warshall
  )
import Painter
  ( fixpt
  , insert
  , union
  , unionMap
  , meet
  , remove
  , subset
  , update
  )
import Data.Tuple (swap)

-- Aufgabe 10.1 a)
{-
I.A.: []
foldList(listT) []
= (\case [] -> nil listT; (x:s) -> cons listT x $ foldList listT s) []
= nil listT;
= [] = id([])

I.V.:
foldList(listT) xs = id(xs)

I.S.:
foldList(listT) x:s
= (\case [] -> nil listT; (x:s) -> cons listT x $ foldList listT s) xs
= cons listT x $ foldList listT s;
= cons listT x $ id s;
= (:) x $ id s; (I.V.)
= (:) x $ s;
= (x:s)
= id (x:s)
-}

-- Aufgabe 10.1 b)
{-
I.A.: []
foldr (cons(alg))(nil(alg)) []
= nil(alg);
= foldList(alg) [];

I.V.:
foldList alg s = foldr (cons alg) (nil alg) s

I.S.:
foldList alg (x:s)
= cons alg x $ foldList alg s;
= cons alg x $ foldr (cons alg) (nil alg) s; (I.V.)
= foldr (cons alg) (nil alg) (x:s);
-}

-- Vorgaben für Aufgabe 10.2 darf nicht geändert werden.
data Mod10 = Z0 | Z1 | Z2 | Z3 | Z4 | Z5 | Z6 | Z7 | Z8 | Z9
  deriving (Enum, Bounded, Eq, Ord)

instance Num Mod10 where
  i1 + i2 = toEnum $ (fromEnum i1 + fromEnum i2) `mod` 10
  i1 * i2 = toEnum $ (fromEnum i1 * fromEnum i2) `mod` 10
  i1 - i2 = toEnum $ (10 + fromEnum i1 - fromEnum i2) `mod` 10
  abs = toEnum . abs . fromEnum
  signum = toEnum . signum . fromEnum
  negate = error "Blatt10.negate: No negative numbers."
  fromInteger i = toEnum $ fromIntegral $ i `mod` 10

instance Show Mod10 where
  show = show . fromEnum

instance Read Mod10 where
  readsPrec p s = [ (toEnum i, s') | (i,s') <- readsPrec p s ]


-- Aufgabe 10.2 a)
f :: Mod10 -> Mod10
f x | x < Z5 = succ x
    | x > Z7 = pred x
    | otherwise = x

-- Aufgabe 10.2 b)
lfp :: Mod10
lfp = fixpt (<=) f Z0

-- Aufgabe 10.2 c)
gfp :: Mod10
gfp = fixpt (>=) f Z9

-- Vorgaben für Aufgabe 10.3 darf nicht geändert werden.
fix :: (a -> a) -> a
fix f = f (fix f)

-- Aufgabe 10.3 a)
lengthF :: [a] -> Int
lengthF = fix phi where
  phi f [] = 0
  phi f (a:es) = 1 + phi f es

-- Aufgabe 10.3 b)
{-
-- Lösung hier einfügen.
-}

-- Vorgaben für Aufgabe 10.4 darf nicht geändert werden.
closureW :: Eq a => Graph a -> Graph a
closureW = warshall


-- Aufgabe 10.4 a)
reverseGraph :: Eq a => Graph a -> Graph a
reverseGraph = undefined -- Durch Lösung ersetzen.

-- Aufgabe 10.4 b)
isReachableFrom :: Eq a => a -> a -> Graph a -> Bool
isReachableFrom = undefined -- Durch Lösung ersetzen.
