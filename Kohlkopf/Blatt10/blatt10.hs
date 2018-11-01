-- Thomas Alessandro Buse 192959, Übung 10, Gruppe 17
-- Paul Rüssmann 196683 

--Aufgabe 10.1
--data STree a = BinS (STree a) a (STree a) | LeftS (STree a) a| RightS a (STree a) | LeafS a deriving Show

data STree a = BinS (STree a) a (STree a) | LeftS (STree a) a| RightS a (STree a) | LeafS a

instance Show a => Show (STree a) where
	show (LeafS a) = show(a)
	show (LeftS (a) b) = show(b) ++ "(" ++ show(a) ++ "," ++ ")"
	show (RightS (a) b) = show(a) ++ "(" ++ "," ++ show(b) ++ ")"
	show (BinS l a r) = show(a) ++ "(" ++ show(l) ++ "," ++ show(r) ++ ")"

testTree = BinS (LeftS (LeafS 9) 2) 4 (RightS 7 (LeafS 3))
testTree1 = LeafS 5
testTree2 = (LeftS (LeafS 9) 2)
testTree3 = (RightS 7 (LeafS 3))
testTree4 = BinS (LeftS (LeftS (LeafS 9) 8) 2) 4 (RightS 7 (LeafS 3))

instance Functor STree where
	fmap f (LeafS a) = (LeafS (f a))
	fmap f (LeftS (a) b) = (LeftS (fmap f a) (f b))-- $ fmap f a 
	fmap f (RightS (a) b) = (RightS (f a) (fmap f b))
	fmap f (BinS l a r) = (BinS (fmap f l) (f a) (fmap f r))
	