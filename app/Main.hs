module Main where

import LSD

main :: IO ()
main = do
  price <- getLine
  let values = read <$> words price :: [Int]
  putPriceCoin (values !! 0) (values !! 1) (values !! 2) (values !! 3)
