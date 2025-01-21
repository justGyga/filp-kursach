{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module EditAd where

import           Data.String            (fromString)
import           Database.SQLite.Simple

data Ad = Ad
  { adId          :: Integer,
    adCost        :: Float,
    adDescription :: String
  }
  deriving (Show)

instance FromRow Ad where
  fromRow = Ad <$> field <*> field <*> field


editAd :: Connection -> Integer -> IO ()
editAd dataBase adId = do
  let adQuery = fromString "SELECT id, cost, description FROM ads WHERE id = ? LIMIT 1;"
  ads <- query dataBase adQuery (Only adId) :: IO [Ad]
  case ads of
    [ad] -> do
      putStrLn "--- Редактирование объявления ---"
      putStrLn "Введите новую стоимость (или Enter чтобы оставить текущую):"
      newCostStr <- getLine
      let newCost = if null newCostStr then adCost ad else read newCostStr

      putStrLn "Введите новое описание (или Enter чтобы оставить текущее):"
      newDesc <- getLine
      let newDescription = if null newDesc then adDescription ad else newDesc

      execute dataBase
        (fromString "UPDATE ads SET cost = ?, description = ? WHERE id = ?")
        (newCost, newDescription, adId)

      putStrLn "Объявление успешно обновлено"

    _ -> putStrLn "Объявление не найдено"




