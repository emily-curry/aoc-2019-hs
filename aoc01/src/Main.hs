module Main (main) where

import AocLib (getPuzzleInput)

main :: IO ()
main = do
  contents <- fmap (map (read :: String -> Int) . lines) getPuzzleInput
  let sumFuel = sum $ map calcFuel contents
  print sumFuel

calcFuel :: Int -> Int
calcFuel a = (a `div` 3) - 2