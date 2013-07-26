module Main where

import System.Environment

quicksort :: Ord a => [a] -> [a]
quicksort (h:t) = quicksort [x | x <- t, x < h] ++ [h] ++ quicksort [x | x <- t, x >= h]
quicksort [] = []
  
main = do
  arg <- System.Environment.getArgs
  case (length arg) of
    0 -> putStrLn "Usage: ./quicksort a b c"
    1 -> putStrLn $ quicksort $ unwords arg
    _ -> putStrLn $ unwords $ quicksort arg
         
         
-- TAIL INFO
-- Name: Quicksort
-- Language: Haskell
-- Compile: ghc --make quicksort.hs
-- State: Done
--
-- Quicksort argv
-- 
-- Example: ./quicksort pandabear

         