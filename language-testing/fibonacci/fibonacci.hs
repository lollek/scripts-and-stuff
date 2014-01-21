module Main where

import System.Environment (getArgs)

fib :: Integer -> String
fib n = fibAux (0, 1, n)
  where
    fibAux :: (Integer, Integer, Integer) -> String
    fibAux (x, _, 0) = show x
    fibAux (x, y, z) = fibAux (y, y + x, z - 1)

main :: IO ()
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
