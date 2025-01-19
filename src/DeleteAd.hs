module DeleteAd where

import Data.String (fromString)
import Database.SQLite.Simple

deleteAd :: Connection -> Integer -> IO ()
deleteAd dataBase adId = do
  -- Получаем тип и ID объекта перед удалением объявления
  let getObjectInfoQuery = Query $ fromString "SELECT \"objectType\", \"objectId\" FROM ads WHERE id = ?"
  objectInfo <- query dataBase getObjectInfoQuery (Only adId) :: IO [(Integer, Integer)]
  case objectInfo of
    [(objType, objId)] -> do
      -- Определяем таблицу на основе типа объекта
      let objectTable = case objType of
            1 -> "flats"
            2 -> "houses" 
            3 -> "landPlot"
            4 -> "garages"
            5 -> "commercialRealEstates"
            _ -> ""
      
      if objectTable /= "" 
        then do
          -- Получаем addressId перед удалением объекта
          let getAddressIdQuery = (fromString $ "SELECT \"addressId\" FROM " ++ objectTable ++ " WHERE id = ?")
          addressIds <- query dataBase (fromString getAddressIdQuery) (Only objId) :: IO [Only Integer]
          -- Удаляем объявление
          execute dataBase (Query $ fromString "DELETE FROM ads WHERE id = ?") (Only adId)
          -- Удаляем объект
          execute dataBase (fromString $ "DELETE FROM " ++ objectTable ++ " WHERE id = ?") (Only objId)
          -- Удаляем адрес
          case addressIds of
            [Only addressId] -> 
              execute dataBase (Query $ fromString "DELETE FROM addresses WHERE id = ?") (Only addressId)
            _ -> return ()
            
          putStrLn "Объявление успешно удалено"
        else putStrLn "Неверный тип объекта"
    _ -> putStrLn "Объявление не найдено"