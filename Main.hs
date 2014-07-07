module Main where

import Cal (currentMonthCal, yearCal)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  cal <- parseArgs args
  putStr cal
  where parseArgs :: [String] -> IO String
        parseArgs [] = currentMonthCal
        parseArgs [y] = return $ yearCal (read y :: Integer)