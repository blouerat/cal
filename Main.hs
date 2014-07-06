module Main where

import Cal (currentCal)

main :: IO ()
main = currentCal >>= putStrLn