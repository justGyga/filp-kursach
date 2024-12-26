{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Menu where

import Ads (filterAvailableAds)
import Commons (clearCLI)
import GetOwnAds (getOwnAds)
import GetProfile (getProfile)
import UserModule (signIn, signUp)
import CreateAd (createAd)

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
  putStrLn "\n----- Меню после аутентификации -----"
  putStrLn "1. Просмотреть доступные варианты к покупке"
  putStrLn "2. Создать объявление"
  putStrLn "3. Просмотреть свой аккаунт"
  putStrLn "4. Просмотреть свои объявления"
  putStrLn "5. Выход"
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
    "5" -> putStrLn "Выход..."
    _ -> do
      putStrLn "Неверный выбор, попробуйте снова."
      afterAuth
