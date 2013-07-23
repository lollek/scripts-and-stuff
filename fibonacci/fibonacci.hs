module Main where

fibAux :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
fibAux (x, y, 0) = (x, y, 0)
fibAux (x, y, z) = fibAux (y, y + x, z - 1)

fibParse :: (Integer, Integer, Integer) -> Integer
fibParse (x, y, z) = x

fib :: Integer -> Integer
fib x = fibParse(fibAux(0, 1, x))
  
-- TAIL INFO
-- Name: Fibonacci Sequence
-- Language: Haskell
-- Compile: :load fibonacci.hs
-- State: Done
--
-- Prints out numbers from the fibonacci sequence
--
--
-- Example: fib 42
--
