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
sizeBintree :: Bintree a -> Int
--sizeBintree = foldTree 1 (+) 0 (+)
sizeBintree = foldBin $ BinSig {empty_ = 0, fork = \a l r -> 1 + l + r }
-- Aufgabe 9.3 b)
{-labelA :: Tree a -> Node -> a
labelA = foldTree $ TreeSig {var_ = , fun = \ -> -}