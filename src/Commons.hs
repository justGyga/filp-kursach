module Commons where

import System.Process (system)

clearCLI :: IO ()
clearCLI = do
  _ <- system "cls"
  putStrLn ""

-- Вспомогательная функция для повторного запроса числового значения
getNumericInput :: (Read a, Ord a, Num a) => String -> (a -> Bool) -> IO (Maybe a)
getNumericInput prompt validator = do
  putStrLn prompt
  input <- getLine
  case reads input of
    [(value, "")] | validator value -> return (Just value)
    _ -> do
      putStrLn "Некорректное значение. Попробуйте еще раз."
      putStrLn "1. Повторить ввод"
      putStrLn "_. Пропустить"
      choice <- getLine
      if choice == "1"
        then getNumericInput prompt validator
        else return Nothing

getInteger :: String -> IO Integer
getInteger prompt = do
  putStrLn prompt
  input <- getLine
  case reads input of
    [(n, "")] ->
      if n > 0
        then return n
        else do
          putStrLn "Пожалуйста, введите положительное число"
          getInteger prompt
    _ -> do
      putStrLn "Пожалуйста, введите корректное целое число"
      getInteger prompt

getFloat :: String -> IO Float
getFloat prompt = do
  putStrLn prompt
  input <- getLine
  case reads input of
    [(n, "")] ->
      if n > 0
        then return n
        else do
          putStrLn "Пожалуйста, введите положительное число"
          getFloat prompt
    _ -> do
      putStrLn "Пожалуйста, введите корректное число"
      getFloat prompt

getString :: String -> IO String
getString prompt = do
  putStrLn prompt
  str <- getLine
  if null str
    then do
      putStrLn "Строка не может быть пустой. Попробуйте еще раз."
      getString prompt
    else return str