module Menu where

import UserModule (signIn, signUp)

startMenu :: IO ()
startMenu = do
  putStrLn "Главное меню"
  putStrLn "1. Войти"
  putStrLn "2. Зарегистрироваться"
  --   putStrLn "3. Просмотреть список вариантов"
  putStrLn "4. Выход"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> signIn
    "2" -> signUp
    -- 3 -> getList
    "4" -> putStrLn "Выход..."
    _ -> putStrLn "Неверный выбор, попробуйте снова." >> startMenu