isCyclic :: Eq a => Graph a -> Bool
	isCyclic a  | isEmpty a = False
            		| not $ hasLeaf a = True
            		| otherwise = isCyclic $ delLeaf a


delLeaf::Eq a => Graph a -> al
	delLeaf a = checkNodes . nodes $ a
		where
		checkNodes :: [Node] -> Bool
		checkNodes [] = False
		checkNodes (x:xs) | leafNode a x = True
                   		| otherwise = checkNodes(xs)

hasLeaf::Eq a => Graph a -> al
	hasLeaf a = checkNodes . nodes $ a
		where
		checkNodes :: [Node] -> Bool
		checkNodes [] = False
		checkNodes (x:xs) | leafNode a x = True
                   		| otherwise = checkNodes(xs)
