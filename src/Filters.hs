{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Filters where

import Commons (getNumericInput)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Enums (allAdObjectTypes)

districtFilter :: Connection -> IO String
districtFilter dataBase = do
  districts <- query_ dataBase "SELECT DISTINCT district FROM addresses;" :: IO [Only Text]

  putStrLn "\n----- Район -----\n"
  putStrLn "1. Выбрать район"
  putStrLn "_. Пропустить"

  districtChoice <- getLine
  case districtChoice of
    "1" -> do
      putStrLn "Доступные районы:"
      mapM_ (\(i, d) -> putStrLn $ show i ++ ". " ++ T.unpack (fromOnly d)) (zip [1 ..] districts)
      n <- getNumericInput "Выберите номер района:" (\x -> x > 0 && x <= fromIntegral (length districts))
      case n of
        Just n -> do
          let selected = lookup n (zip [1 ..] (map fromOnly districts))
          case selected of
            Just d -> return $ " AND addresses.district='" ++ T.unpack d ++ "'"
            Nothing -> putStrLn "Неверный номер района." >> return ""
        Nothing -> do
          putStrLn "Некорректный ввод. Пожалуйста, введите число."
          return ""
    _ -> return ""

objectTypeFilter :: IO String
objectTypeFilter = do
  putStrLn "\n----- Тип объекта -----\n"
  putStrLn "1. Выбрать тип объекта"
  putStrLn "_. Пропустить"
  objectTypeChoice <- getLine
  case objectTypeChoice of
    "1" -> do
      putStrLn "Доступные типы объектов:"
      mapM_ (\(i, t) -> putStrLn $ show i ++ ". " ++ show t) (zip [1..] allAdObjectTypes)
      n <- getNumericInput "Выберите номер типа:" (\x -> x > 0 && x <= fromIntegral (length allAdObjectTypes))
      case n of
        Just n -> do
          let selected = lookup n (zip [1..] allAdObjectTypes)
          case selected of
            Just t -> return $ " AND ads.\"objectType\"='" ++ show t ++ "'"
            Nothing -> putStrLn "Неверный номер типа." >> return ""
        Nothing -> do
          putStrLn "Некорректный ввод. Пожалуйста, введите число."
          return ""
    _ -> return ""

minimalCostFilter :: IO String
minimalCostFilter = do
  putStrLn "\n----- Минимальная стоимость -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "_. Пропустить"
  minCostChoice <- getLine
  case minCostChoice of
    "1" -> do
      putStrLn "Введите минимальную стоимость:"
      input <- getLine
      case reads input :: [(Float, String)] of
        [(cost, "")] | cost >= 0 -> return $ " AND ads.cost >= " ++ show cost
        _ -> do
          putStrLn "Некорректное значение. Пожалуйста, введите положительное число."
          return ""
    _ -> return ""

maxCostFilter :: IO String
maxCostFilter = do
  putStrLn "\n----- Максимальная стоимость -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "_. Пропустить"
  maxCostChoice <- getLine
  case maxCostChoice of
    "1" -> do
      putStrLn "Введите максимальную стоимость:"
      input <- getLine
      case reads input :: [(Float, String)] of
        [(cost, "")] | cost >= 0 -> return $ " AND ads.cost <= " ++ show cost
        _ -> do
          putStrLn "Некорректное значение. Пожалуйста, введите положительное число."
          return ""
    _ -> return ""

minAreaFilter :: IO String
minAreaFilter = do
  putStrLn "\n----- Минимальная площадь объекта -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "_. Пропустить"
  minAreaChoice <- getLine
  case minAreaChoice of
    "1" -> do
      putStrLn "Введите минимальную площадь объекта:"
      input <- getLine
      case reads input :: [(Int, String)] of
        [(area, "")] | area >= 0 -> return $ " AND objs.area >= " ++ show area
        _ -> do
          putStrLn "Некорректное значение. Пожалуйста, введите положительное целое число."
          return ""
    _ -> return ""

maxAreaFilter :: IO String
maxAreaFilter = do
  putStrLn "\n----- Максимальная площадь объекта -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "_. Пропустить"
  maxAreaChoice <- getLine
  case maxAreaChoice of
    "1" -> do
      putStrLn "Введите максимальную площадь объекта:"
      input <- getLine
      case reads input :: [(Int, String)] of
        [(area, "")] | area >= 0 -> return $ " AND objs.area <= " ++ show area
        _ -> do
          putStrLn "Некорректное значение. Пожалуйста, введите положительное целое число."
          return ""
    _ -> return ""
