module WalletModule where

import Database.SQLite.Simple

createWallet :: Connection -> IO Int
createWallet db = do
  wallets <- query_ db "SELECT id FROM wallets ORDER BY id DESC LIMIT 1;" :: IO [Only Int]
  let id =
        if null wallets
          then 1
          else let Only lastId = head wallets in lastId + 1 
  execute db "INSERT INTO wallets (id, balance) VALUES (?, 0)" (Only id)
  return id
