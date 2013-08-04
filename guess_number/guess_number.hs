module Main where

import qualified System.Random as R
import qualified System.IO as IO

guessNumber :: (Integer, Integer) -> IO ()
guessNumber (solution, num)
  | num == 6 = putStrLn $ "Haha, I won! The number was " ++ show solution
  | otherwise = do
    putStr $ "Guess " ++ show num ++ ": "
    IO.hFlush IO.stdout
    guess <- getLine
    case (read guess) `compare` solution of
      LT -> do
        putStrLn "Too low! Try again!"
        guessNumber(solution, succ num)
      GT -> do
        putStrLn "Too high! Try again!"
        guessNumber(solution, succ num)
      EQ -> do
        putStrLn "Correct! You have won!"

main = do
  solution <- R.randomRIO(1, 100) :: IO Integer
  putStrLn "Guess-a-number game!"
  putStrLn "I am thinking of a number between 1 and 100"
  putStrLn "You have 5 tries to guess it correctly or I win"
  putStrLn "What's your guess?"
  guessNumber(solution, 1)
  
-- TAIL INFO:
-- Name: Guess number
-- Language: Haskell
-- Compile: ghc --make guess_number.hs
-- State: Done
--
-- Play guess-a-number game
--
-- Example: ./guess_number
