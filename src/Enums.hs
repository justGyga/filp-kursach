{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Enums where

getAreaType :: IO Integer
getAreaType = do
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
      getAreaType


