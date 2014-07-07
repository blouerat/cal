module Main where

import Cal (currentMonthCal, monthCal, yearCal)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  cal <- parseArgs args
  putStr cal
  where parseArgs :: [String] -> IO String
        parseArgs [] = currentMonthCal
        parseArgs [y] = return $ yearCal (read y)
        parseArgs [m, y] = return . unlines $ monthCal True (read y) (read m)