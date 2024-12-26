{-# LANGUAGE OverloadedStrings #-}

module GetDeals where

import Database.SQLite.Simple
    ( Connection, query, field, FromRow(..), Query(Query) )

data Deal = Deal {id :: Integer, buyer :: Integer, adId :: Integer, status :: String, date :: String, finalCost :: Float, seller :: Integer, description :: String, name :: String, surname :: String} deriving (Show)

instance FromRow Deal where
  fromRow = Deal <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
getDeals :: Connection -> Integer -> IO ()
getDeals db id = do
  let dealsQuery = Query $ mconcat
        [ "SELECT deals.id, buyer, \"adId\", status, date, \"finalCost\", seller, description, name, surname "
        , "FROM deals "
        , "JOIN ads ON ads.id = deals.\"adId\" "
        , "JOIN users ON seller = users.id "
        , "WHERE seller = ? OR buyer = ? "
        , "ORDER BY date"
        ]
  deals <- query db dealsQuery (id, id) :: IO [Deal]
  case deals of
    [] -> putStrLn "Нет сделок"
    _ -> do
      putStrLn "\n--- Сделки ---"
      mapM_ (\deal -> do
        putStrLn $ "ID сделки: " ++ show (GetDeals.id deal)
        putStrLn $ "Покупатель: " ++ show (buyer deal)
        putStrLn $ "Объявление: " ++ show (adId deal)
        putStrLn $ "Статус: " ++ status deal
        putStrLn $ "Дата: " ++ date deal
        putStrLn $ "Итоговая стоимость: " ++ show (finalCost deal)
        putStrLn $ "Продавец: " ++ name deal ++ " " ++ surname deal
        putStrLn $ "Описание: " ++ description deal
        ) deals
