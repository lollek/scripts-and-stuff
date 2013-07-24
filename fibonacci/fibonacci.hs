module Main where

import System.Environment

fibAux :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
fibAux (x, y, 0) = (x, y, 0)
fibAux (x, y, z) = fibAux (y, y + x, z - 1)

fibParse :: (Integer, Integer, Integer) -> Integer
fibParse (x, y, z) = x

fib :: Integer -> Integer
fib x = fibParse(fibAux(0, 1, x))

getArg :: [String] -> Integer
getArg arg = read (arg !! 0) :: Integer

main = do
  arg <- System.Environment.getArgs
  if length arg == 1 then
    print $ fib $ getArg arg
    else putStrLn "Usage: ./fibonacci <number>"

  
-- TAIL INFO
-- Name: Fibonacci Sequence
-- Language: Haskell
-- Compile: ghc --make fibonacci.hs
-- State: Done
--
-- Prints out numbers from the fibonacci sequence
--
--
-- Example: ./fibonacci 42
--
