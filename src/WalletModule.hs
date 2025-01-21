{-# LANGUAGE OverloadedStrings #-}

module WalletModule where

import           Database.SQLite.Simple

createWallet :: Connection -> IO Int
createWallet db = do
  let queryStr = "SELECT id FROM wallets ORDER BY id DESC LIMIT 1;"
  wallets <- query_ db queryStr :: IO [Only Int]
  let newWalletId =
        if null wallets
          then 1
          else let Only lastId = head wallets in lastId + 1
  execute db "INSERT INTO wallets (id, balance) VALUES (?, 0)" (Only newWalletId)
  return newWalletId
