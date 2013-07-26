module Main where

import System.Environment (getArgs)

split :: String -> String -> [String]
split _ [] = [""]
split w (x:xs)
  | take (length w) (x:xs) == w = "" : split w (drop (length w) (x:xs))
  | otherwise = (x: head (split w xs)) : tail (split w xs)

main = do
  argv <- getArgs
  if length argv > 1 then
    let (x:xs) = argv in putStrLn $ unwords $ split x $ unwords xs
    else putStrLn "Usage: ./haskell_split delim string"
         
 -- TAIL INFO:
-- Name: Split
-- Language: Haskell
-- Compile: ghc --make haskell_split.hs
-- State: Done
--
-- split "e" "hello" == ["h","llo"]
-- did this to replace splitOn from Data.Text but later changed my mind
--
-- Example: ./haskell_split e hello world
-- This is not meant to be run though
