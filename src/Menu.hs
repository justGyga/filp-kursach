{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Menu where

import           Commons     (clearCLI)
import           CreateAd    (createAd)
import           GetAdsList  (filterAvailableAds)
import           GetOwnAds   (getOwnAds)
import           GetOwnDeals (getOwnDeals)
import           GetProfile  (getProfile)
import           UserModule  (signIn, signUp)

startMenu :: IO ()
startMenu = do
  putStrLn "\nГлавное меню"
  putStrLn "1. Войти"
  putStrLn "2. Зарегистрироваться"
  putStrLn "3. Выход"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> do
      success <- signIn
      if success
        then afterAuth
        else startMenu
    "2" -> do
      success <- signUp
      if success
        then afterAuth
        else startMenu
    "3" -> putStrLn "Выход..."
    _ -> do
      putStrLn "Неверный выбор, попробуйте снова."
      startMenu

afterAuth :: IO ()
afterAuth = do
  putStrLn "\n----------- Меню -----------"
  putStrLn "1. Просмотреть доступные варианты к покупке"
  putStrLn "2. Создать объявление"
  putStrLn "3. Просмотреть свой аккаунт"
  putStrLn "4. Просмотреть свои объявления"
  putStrLn "5. Просмотреть свои сделки"
  putStrLn "6. Выход"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> do
      clearCLI
      filterAvailableAds
      afterAuth
    "2" -> do
      clearCLI
      createAd
      afterAuth
    "3" -> do
      clearCLI
      getProfile
      afterAuth
    "4" -> do
      clearCLI
      getOwnAds
      afterAuth
    "5" -> do
      clearCLI
      getOwnDeals
      afterAuth
    "6" -> putStrLn "Выход..."
    _ -> do
      putStrLn "Неверный выбор, попробуйте снова."
      afterAuth
