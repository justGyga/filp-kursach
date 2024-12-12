{-# LANGUAGE OverloadedStrings #-}

import SQLplotter (getUserSession)
import Database.SQLite.Simple


data Address = Address {id: Integer}

getMyAddresses :: IO ()
getMyAddresses = do
  dataBase <- open "local.db"
  myId <- getUserSession
  getAddressesByUser dataBase myId
  close dataBase


getAddressesByUser :: Connection -> Integer -> IO ()
getAddressesByUser db myId = do
  addresses <- query db "SELECT * FROM ads WHERE seller = ?;" (Only id)

