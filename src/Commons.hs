module Commons where

import System.Process (system)

clearCLI :: IO ()
clearCLI = do
  _ <- system "cls"
  putStrLn ""