module Main where

import Commons (clearCLI)
import Control.Monad (when)
import Menu (startMenu)
import SQLplotter (initializeDB, seedDB)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when ("init-db" `elem` args) $ do
    initializeDB
    seedDB
  clearCLI
  startMenu

--   putStrLn "Hello, CLI!"
