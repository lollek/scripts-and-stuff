module Main where

import System.Environment (getArgs)

fib :: Integer -> String
fib x = fibAux (0, 1, x)
  where
    fibAux (x, y, 0) = show 0
    fibAux (x, y, z) = fibAux (y, y + x, z - 1)

main = do
  argv <- getArgs
  if length argv == 1 then
    putStrLn $ fib $ read $ unwords argv
    else
    putStr $ unlines $ map fib [0..19]

  
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
