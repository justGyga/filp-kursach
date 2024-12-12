module Main where

import Commons (clearCLI)
import Control.Monad (when)
import Menu (startMenu)
import SQLplotter (initializeDB, seedDB, addSessionTable)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when ("init-db" `elem` args) $ do
    initializeDB
    seedDB
  addSessionTable
  clearCLI
  startMenu

--   putStrLn "Hello, CLI!"
