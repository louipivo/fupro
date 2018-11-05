module Blatt02 where

import Examples

-- Aufgabe 2.1 a)
{-
(\x y -> y x) ab
~> (\x -> \y -> y x) a b
~> (\x -> \y -> y a)
~> (\x -> \y -> b a)
~> b a
-}

-- Aufgabe 2.1 b)
{-

(\ y z -> ) ((\x -> x x) ( \x -> x x )) a
~> (\y ->\z -> z) (( \x -> x x)(\x -> x x)) a
~> (\z->z)a
~> a

-}


-- Aufgabe 2.1 c)
{-

~>(\f g x -> f(g x)) (\y -> y y ) (\z -> a)
~>(\f -> \g -> \x -> f(g x)) (\y - y y) (\z -> a)
~>\g -> \x -> ((\y - y y)(g x)) (\z -> a)
~>\x -> ((\y -> y y)((\z -> a) x)
~>\x -> ((\z -> a) x) ((\z -> a) x))
~>(\x -> (a((\z->a)x)))
~>(\x -> (a a))

-}





-- Aufgabe 2.2 a)
f :: Either Float Point -> Either Float Point
f = \e -> case e of
  (Left x y)  -> x+y
  (Right pt) -> x(pt)+y(pt)

-- Aufgabe 2.2 b)
g :: Int -> Int -> Int
g x y = if even x then x * y else if x > 50 && y > 100 && even x == False then x - y else if y > 0 && even x == False then x `div` y else x + y










-- Aufgabe 2.3 a)
ausdruckA x y z =  x + y + 5 * z
klammernA x y z = (x + (y + (5 * z))) -- Klammern hier setzen.
praefixA x y z  = (+) x ((+) y ((*) 5 z)) -- Präfixdarstellung hier einfügen.

--Aufgabe 2.3 b)
ausdruckB f g h x = f . g $ h $ f x
klammernB f g h x =  (f . g ($ h $ f x)) -- Klammern hier setzen.
praefixB f g h x  = (.) f g (($) h (($)f x)-- Präfixdarstellung hier einfügen.

--Aufgabe 2.3 c)
ausdruckC f = f 5 True 3
klammernC f = (((f 5) True) 3) -- Klammern hier setzen.
praefixC f  = (f) 5 True 3  -- Präfixdarstellung hier einfügen.






-- Aufgabe 2.4 a)
(||):: Bool -> Bool -> Bool
(||)  = \a -> \b -> (case a of
    True -> True
    False -> b)


 --False || True schrittwiese auswerten.
(||)  = \a -> \b -> (case a of
  True -> True
  False -> b) False True

  ~> \b -> (case Flase of
    True -> True
    False -> b) True

    ~> (case Flase of
      True -> True
      False -> True)

      ~> True

--Aufgabe 2.4 b)s
(.) :: (b -> c) -> (a -> b) -> a -> c
      (g . f) a = g (f a)
      (.) = (\g -> \f) -> \a


{-   h . h' schrittwiese auswerten.
-}
