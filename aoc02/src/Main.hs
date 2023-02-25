module Main (main) where

import AocLib (getPuzzleInput)
import IntCode

main :: IO ()
main = do
  contents <- getPuzzleInput
  let ic = withArgs 12 2 $ fromString contents
  let (result, _) = run ic
  putStrLn $ "Result of intcode program with args 12,2: " ++ show result
  let ic2 = fromString contents
  let (noun, verb) = getArgs $ withResult 19690720 ic2
  putStrLn $ "Args for intcode program with result 19690720: " ++ show noun ++ "," ++ show verb ++ " computed answer: " ++ show ((noun * 100) + verb)