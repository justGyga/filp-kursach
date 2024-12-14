-- {-# LANGUAGE OverloadedStrings #-}

module Ads where

import SQLplotter (getUserSession)
import Database.SQLite.Simple


data Ad = Ad {id:: Integer, seller:: Integer, objectId:: Integer, objectType:: Integer, cost:: Float, description:: String}
instance FromRow Ad where
  fromRow = Ad <$> field <*> field <*>field <*> field <*> field <*> field <*> field
instance ToRow Ad where
  toRow(Ad id seller objectId objectType cost description) = toRow (id, seller, objectId, objectType, cost, description)

getMyAds ::  IO ()
getMyAds = do
  dataBase <- open "local.db"
  myId <- getUserSession
  getAdsByUser dataBase myId
  close dataBase


getAdsByUser :: Connection -> Integer -> IO ()
getAdsByUser db myId = do
  addresses <- query db "SELECT * FROM ads WHERE seller = ?;" (id) :: IO [Ad]
  print addresses

