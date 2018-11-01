bexp2store :: BExp -> BStore x -> Store x -> Bool
bexp2store True_ _ _ = True
bexp2store False_ _ _ = False
bexp2store (BVar x) bst _ = bst x

bexp2store (Or bs) bst st = or $ map (\x -> bexp2store x bst st) bs
bexp2store (And bs) bst st = and $ map (\x -> bexp2store x bst st) bs
bexp2store (Not bs) bst st = not $ bexp2store bs bst st
bexp2store (e1 := e2) _ st = exp2store e1 st == exp2store e2 st
bexp2store (e1 :<= e2) _ st = exp2store e1 st <= exp2store e2 st
