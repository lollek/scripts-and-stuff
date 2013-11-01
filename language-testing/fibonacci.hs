module Main where

import System.Environment (getArgs)

fib :: Integer -> String
fib x = fibAux (0, 1, x)
  where
    fibAux (x, y, 0) = show x
    fibAux (x, y, z) = fibAux (y, y + x, z - 1)

main = do
  argv <- getArgs
  if length argv == 1 then
    putStrLn $ fib $ read $ unwords argv
    else
    mapM_ (\x -> putStrLn (fib x ++ "\t" ++ fib (x+10))) [0..9]
  
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