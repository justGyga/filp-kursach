{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Enums where

getHouseAreaType :: IO Integer
getHouseAreaType = do
  putStrLn "--------- Выберите тип площади --------"
  putStrLn "1. Дом для постоянного проживания"
  putStrLn "2. Дача"
  putStrLn "Введите выбранный пункт меню:"
  choice <- getLine
  case choice of
    "1" -> return 1
    "2" -> return 2
    _ -> do
      putStrLn "Неверный выбор, попробуйте снова."
      getHouseAreaType

getLandCategory :: IO Integer
getLandCategory = do
  putStrLn "--------- Выберите категорию земельного участка --------"
  putStrLn "1. Жилой участок"
  putStrLn "2. Коммерческий участок"
  putStrLn "3. Сельхоз участок"
  putStrLn "4. Промышленный участок"
  putStrLn "Введите выбранный пункт меню:"
  choice <- getLine
  case choice of
    "1" -> return 1
    "2" -> return 2
    "3" -> return 3
    "4" -> return 4
    _ -> do
      putStrLn "Неверный выбор, попробуйте снова."
      getLandCategory

