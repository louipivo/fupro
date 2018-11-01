type ID = Int
data Bank = Bank [Account] deriving Show
data Account = Account {balance :: Int, owner :: Client} deriving Show
data Client = Client
  {name :: String
  , surname :: String
  , address :: String
  } deriving Show

cl1, cl2 :: Client
cl1 = Client "Harry" "Potter" "London"
cl2 = Client "Homer" "Simpson" "Springfield"

konto1, konto2 :: Account
konto1 = Account 666 cl1
konto2 = Account 6 cl2

bank :: Bank
bank = Bank [konto1, konto2]

credit :: Int -> ID -> Bank -> Bank
credit amount id (Bank ls)
  = Bank(updList ls id entry{balance = altBalance + amount})
  where
    entry = ls !! id
    altBalance = balance entry

debit :: Int -> ID -> Bank -> Bank
debit amount = credit(-amount)

transfer :: Int -> ID -> ID -> Bank -> Bank
transfer amount id1 id2 = debit amount id1 . credit amount id2
