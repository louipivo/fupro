
-- 30.1.2016

module Align where

import Control.Monad
import Data.Array
import Data.Maybe
import Movie(Pos)
import Painter(CLPath,readFileAndDo,drawLGraph,RGB,red,green,blue,white,float)
		
-- download http://fldit-www.cs.uni-dortmund.de/~peter/Haskellprogs/Painter.tgz
 
third (_,_,c) = c                  
 
maxima :: Ord b => (a -> b) -> [a] -> [a]
maxima f s = filter ((== m) . f) s where m = maximum $ map f s
		 
type Align  = [Triple]
type Triple = (Maybe String,Maybe String,RGB)

-- matchcount ali counts the matches of ali.
-- maxmatch ali computes the length of the maximal matches of ali.

matchcount,maxmatch :: Align -> Int
matchcount = length . filter (/= white) . map third 
maxmatch ali = max i m 
	       where (_,i,m) = foldl trans (False,0,0) $ map third ali
                     trans (b,i,m) c = if c == white then (False,0,max i m)
                                       else (True,if b then i+1 else 1,m)

consx,consy :: String -> Align -> Align
consx x ali = (Just x,Nothing,white):ali
consy y ali = (Nothing,Just y,white):ali

equal,match :: String -> String -> Align -> Align
equal x y ali = (Just x,Just y,green):ali
match x y ali = (Just x,Just y,red):ali

mkArray :: Ix a => (a,a) -> (a -> b) -> Array a b 
mkArray bds f = array bds [(i,f i) | i <- range bds]

compl :: String -> String -> Bool
compl x y = f x y || f y x where
            f x y = x == "a" && y == "t" || x == "c" && y == "g"
            
data MatAlg a m = M {first :: (a,a), next :: a -> a, maxi,maxj :: a,
		     getx,gety :: a -> String,
		     entry :: m -> (a,a) -> [Align],
		     mkMat :: ((a,a) -> [Align]) -> m}

type Genes = ([String],[String])

lists :: Genes -> MatAlg [String] (Genes -> [Align])
lists gs = M {first = gs, next = tail, maxi = [], maxj = [], 
	      getx = head, gety = head, entry = ($), mkMat = id}

fun :: Genes -> MatAlg Int (Pos -> [Align])
fun (xs,ys) = M {first = (1,1), next = (+1), 
		 maxi = length xs+1, maxj = length ys+1, 
		 getx = \i -> xs!!(i-1), gety = \i -> ys!!(i-1), 
		 entry = ($), mkMat = id}

arr :: Genes -> MatAlg Int (Array Pos [Align])
arr (xs,ys) = M {first = (1,1), next = (+1), maxi = maxi, maxj = maxj,  
	         getx = \i -> xs!!(i-1), gety = \i -> ys!!(i-1), 
	         entry = (!), mkMat = mkArray ((1,1),(maxi,maxj))}
              where maxi = length xs+1; maxj = length ys+1

-- maxAlis mat computes all alignments with maximal matchcount value und chooses
-- all connected matches of maximal length.
		  	
maxAlis :: Eq a => MatAlg a m -> [Align]
maxAlis mat = maxima maxmatch $ entry mat align $ first mat
              where align = mkMat mat $ maxima matchcount . f
                    f (i,j) | b         = if c then [[]] else alis4
                 	    | c	        = if b then [[]] else alis3
                 	    | x == y    = alis1++alis3++alis4
                 	    | compl x y = alis2++alis3++alis4 
                 	    | True      = alis3++alis4
                 	      where b  = i == maxi mat; c  = j == maxj mat
                 	            x  = getx mat i;    y  = gety mat j
                 	            ni = next mat i;    nj = next mat j; 
                 	            alis1 = g (equal x y) (ni,nj) 
                 	            alis2 = g (match x y) (ni,nj)
                 	            alis3 = g (consx x) (ni,j)
                 	            alis4 = g (consy y) (i,nj)
                    g cons = map cons . entry mat align

drawAlis :: String -> Int -> IO ()
drawAlis file n = readFileAndDo file f where
		  alignFile = file++"Align"
   	          f str = do writeFile alignFile $ show $ concat 
   	          				 $ zipWith mkPaths [0..] alis
		     	     drawLGraph alignFile 3
		     	  where (str1,str2) = break (== '\n') str
		     	        gs = (words str1,words str2)
		     	        alis = case n of 0 -> maxAlis $ lists gs
		     	              		 1 -> maxAlis $ fun gs
		     	              		 _ -> maxAlis $ arr gs
		        
			        			       	 
mkPaths :: Int -> Align -> [CLPath]                -- siehe Painter
mkPaths j ali = hor 0:hor 1:zipWith ver cols [0..]
                where (s1,s2,cols) = unzip3 ali
		      hor k = (blue,[p "" 0 k,p "" (length s1-1) k])
		      ver col i = (col,[q s1 i 0,q s2 i 1])
		      p str i k = (str,float i,float $ j+j+k)
		      q s i = p (case s!!i of Just a -> a; _ -> "") i
	  
-- drawAlis "ali1" n    a c t a c t g c t    a g a t a g
-- drawAlis "ali2" n    a d f a a a a a a    a a a a a a d f a
-- drawAlis "ali3" n    a b c d s e g f      f a s b c d e

