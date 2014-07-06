module Main where

import Cal (currentMonthCal)

main :: IO ()
main = currentMonthCal >>= putStr