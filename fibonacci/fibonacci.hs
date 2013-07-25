module Main where

import System.Environment

fibAux :: (Integer, Integer, Integer) -> String
fibAux (x, y, 0) = show x
fibAux (x, y, z) = fibAux (y, y + x, z - 1)

fib :: String -> String
fib x = fibAux (0, 1, read x :: Integer)

main = do
  argv <- getArgs
  if length argv == 1 then
    let (a:b) = argv
    in putStrLn $ fib a
    else
    putStr $ unlines $ map fib [show x | x <- [0..19]]

  
-- TAIL INFO
-- Name: Fibonacci Sequence
-- Language: Haskell
-- Compile: ghc --make fibonacci.hs
-- State: Done
--
-- Prints out numbers from the fibonacci sequence
--
-- Example: ./fibonacci
-- Example2: ./fibonacci 42
--
