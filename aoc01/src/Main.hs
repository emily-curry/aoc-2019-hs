module Main (main) where

import AocLib (getPuzzleInput)

main :: IO ()
main = do
  contents <- fmap (map (read :: String -> Int) . lines) getPuzzleInput
  let sumFuel = sum $ map calcFuel contents
  putStrLn $ "Part 1: " ++ show sumFuel
  let sumFuelRecursive = sum $ map calcFuelR contents
  putStrLn $ "Part 2: " ++ show sumFuelRecursive

calcFuel :: Int -> Int
calcFuel mass = (mass `div` 3) - 2

calcFuelR :: Int -> Int
calcFuelR mass
  | fuel <= 0 = 0
  | otherwise = fuel + calcFuelR fuel
  where
    fuel = calcFuel mass