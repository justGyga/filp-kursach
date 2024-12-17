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