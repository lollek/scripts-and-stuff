module Main where

import System.Random
import System.IO

guess_number :: (Integer, Integer) -> IO ()
guess_number (solution, 6) = 
  putStrLn ("Haha, I won! The number was " ++ show solution)
guess_number (solution, num) = do
  putStr ("Guess " ++ show num ++ ": ")
  System.IO.hFlush stdout
  guess <- getLine
  case (read guess) `compare` solution of
    LT -> do
      putStrLn "Too low! Try again!"
      guess_number(solution, succ num)
    GT -> do
      putStrLn "Too high! Try again!"
      guess_number(solution, succ num)
    EQ -> do
      putStrLn "Correct! You have won!"


main = do
  solution <- System.Random.randomRIO(1, 100) :: IO Integer
  putStrLn "Guess-a-number game!"
  putStrLn "I am thinking of a number between 1 and 100"
  putStrLn "You have 5 tries to guess it correctly or I win"
  putStrLn "What's your guess?"
  guess_number(solution, 1)
  
-- TAIL INFO:
-- Name: Guess number
-- Language: Haskell
-- Compile: ghc --make guess_number.hs
-- State: Done
--
-- Play guess-a-number game
--
-- Example: ./guess_number
