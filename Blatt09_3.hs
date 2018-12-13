module Blatt09_3 where

import Painter
  ( Tree(..)
  , Node
  )
import Coalg
  ( Bintree(..)
  , BinSig(..)
  , TreeSig(..)
  , foldBin
  , foldTree
  , tree1
  , tree2
  , btree1
  , btree4
  )

-- Aufgabe 9.3 a)
{-sizeBintree :: Bintree a -> Int
sizeBintree (Fork a b c) = (sizeBintree b) + (sizeBintree c) + 1
sizeBintree (Empty) = 0-}
sizeBintree :: Bintree a -> Int
sizeBintree = foldBin 

-- Aufgabe 9.3 b)
labelA :: Tree a -> Node -> a
labelA = undefined -- Durch Lösung ersetzen.
