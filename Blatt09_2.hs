module Blatt09_2 where

import Painter
  ( Tree(..)
  , Node
  )
import Coalg
  ( tree1
  , tree2
  )

-- Aufgabe 9.2 a)
productA :: Tree Int -> Int
productA (F a ts) = a * (map productA ts)
productA _ = 1
--  Aufgabe 9.2 b)
{-
preorderT :: Tree a -> [a]
preorderT (F a ts) = a : concatMap (preorderT ts)
preorderT (V a) = a
preorderT _ = []-}