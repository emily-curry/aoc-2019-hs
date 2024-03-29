module AocLib
  ( getPuzzleInput,
    getPuzzleInputFromFilename,
  )
where

import System.Environment (getExecutablePath, getProgName)
import System.FilePath (joinPath, splitDirectories)

repoDir :: String
repoDir = "aoc-2019-hs"

getPuzzleInput :: IO String
getPuzzleInput = do
  fname <- getInputFilename
  getPuzzleInputFromFilename fname

getPuzzleInputFromFilename :: String -> IO String
getPuzzleInputFromFilename fname = do
  path <- getPathname fname
  readFile path

getInputFilename :: IO String
getInputFilename = do
  progName <- getProgName
  let r = case drop 3 progName of
        "lib-test" -> "lib"
        x -> x
  return $ r ++ ".txt"

getPathname :: String -> IO String
getPathname f = do
  basePath <- getExecutablePath
  let r = joinPath . (++ ["input", f]) . takeUntilInclusive (== repoDir) . splitDirectories $ basePath
  return r

takeUntilInclusive :: (a -> Bool) -> [a] -> [a]
takeUntilInclusive _ [] = []
takeUntilInclusive p (x : xs)
  | p x = [x]
  | otherwise = x : takeUntilInclusive p xs