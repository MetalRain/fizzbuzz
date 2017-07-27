module Main where

import Control.Applicative 
import Data.Maybe
import Data.Monoid

fizzbuzzes = map fromJust $ zipWith3 choose fizzes buzzes numbers where
  choose f b n = f <> b <|> n
  everyN n s   = cycle $ replicate (n - 1) Nothing <> [Just s]
  fizzes       = everyN 3 "Fizz"
  buzzes       = everyN 5 "Buzz"
  numbers      = map (Just . show) [1..]

main :: IO ()
main = print $ take 60 $ fizzbuzzes
