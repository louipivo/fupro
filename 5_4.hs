updRel :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
updRel ((a,b):r) c d = if a == c then (a,d):r else (a,b):updRel r c d
updRel _ a b = [(a,b)]

type ID = Int
type Bank = [(ID,Account)]
data Account = Account {balance :: Int, owner :: Client}
data Client = Client
  {name :: String
  , surname :: String
  , address :: String}

credit :: Int -> ID -> Bank -> Bank
credit amout id (Bank ls)
  = Bank (updRel ls id entry {balance = oldBalance + amount})
  where
    Just enty = lookup id ls
    oldBalance = balance entry
