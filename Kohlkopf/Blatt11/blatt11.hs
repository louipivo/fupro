--Thomas Alessandro Buse; 192959; Übung 11
--Paul Rüssmann 196683
{-# LANGUAGE LambdaCase #-}
import Expr
import Control.Monad
--Aufgabe 11.1
{-
bexp2store :: BExp x -> Store x -> BStore x -> Bool
bexp2store True_ _ _ = True
bexp2store False_ _ _ = False
bexp2store (BVar x) _ bst = bst x
bexp2store (Or bs) st bst = or $ map (\x -> bexp2store x st bst) bs
bexp2store (And bs) st bst = and $ map (\x -> bexp2store x st bst) bs
bexp2store (Not b) st bst = not $ bexp2store b st bst
bexp2store (e1 := e2) st _ = exp2store e1 st == exp2store e2 st
bexp2store (e1 :<= e2) st _ = exp2store e1 st <= exp2store e2 st
-}
{-
type BStore x = x -> Bool
bexp2store :: BExp x -> Store x -> BStore x -> Bool
bexp2store = 
	\case 
	True _ -> return True
	False _ -> return False
	(BVar x) bst -> ($bst x)
	(Or bs) bst -> do is <- mapM bexp2store es; return $ or is
	(And bs) bst -> do is <- mapM bexp2store es; return $ and is
	(Not b) bst -> do is <- mapM bexp2store es; return $ not is
	(e1 := e2) _ -> bexp2store e1 == bexp2store e2
	(e1 :<= e2) _ ->  bexp2store e1 <= bexp2store e2
-}
{-
type BStore x = x -> Bool
bexp2store :: BExp x -> Store x -> BStore x -> Bool
bexp2store (True_) _ = return True
bexp2store (False_) _ = return False
bexp2store (BVar x) _ = ($x)
bexp2store (Or bs) st = do is <- mapM (\x -> bexp2store x st) bs; return $ or is
bexp2store (And bs) st = do is <- mapM (\x -> bexp2store x st) bs; return $ and is
bexp2store (Not bs) st = do is <- bexp2store bs st; return $ not is
bexp2store (e1 := e2) st = do k <- st(exp2store e1); return $ k
-}
--Aufgabe 10.2

type ID = Int
type Bank = [(ID,Account)]
data Account = Account { balance :: Int, owner :: Client } deriving Show
data Client = Client
    { name :: String
    , surname :: String
    , address :: String
    } deriving Show

own1, own2, own3 :: Client
own1 = Client "Max" "Mustermann" "Musterhausen"
own2 = Client "John" "Doe" "Somewhere"
own3 = Client "Erika" "Mustermann" "Musterhausen"

acc1, acc2, acc3 :: Account
acc1 = Account 100 own1
acc2 = Account 0 own2
acc3 = Account 50 own3

bank :: Bank
bank = [(1,acc1), (2,acc2), (3,acc3)]
{-
credit :: Int -> ID -> Bank -> Bank
credit amount id ls
	= updRel ls id entry{ balance = oldBalance + amount} where
	Just entry = lookup id ls
	oldBalance = balance entry

debit :: Int -> ID -> Bank -> Bank
debit amount = credit (-amount)
-}

--Aufgabe 11.3
newtype State state a = State {runS :: state -> (a,state)}
--a)
putAccount :: ID -> Account -> State Bank ()
putAccount id acc = State $ \_ -> ((),[(id,acc)])

--b)
getAccount :: ID -> State Bank (Maybe Account)
getAccount id = undefined
--c)
creditS :: Int -> ID -> State Bank ()
creditS amount id = do
	Just (Account balance owner) <- getAccount id
	putAccount id (Account (balance + amount) owner)