import Data.Array
import Data.Ix

bincoeffDyn :: (Int, Int) -> Int
bincoeffDyn (n,k) = bincoeffArr !(n,k) where
bincoeffArr :: Array Int Int
bincoeffArr = mkArray ((0,0),(n,n)) binCalc
binCalc :: (Int,Int) -> Int
binCalc(n,k)
  | k == 0 || k == n = 1
  | 0 < k, k < n = bincoeffDyn(n-1,k-1) + bincoeffDyn(n-1,k)
  | otherwise = 0
