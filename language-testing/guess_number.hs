module Main where

import qualified System.Random as R
import qualified System.IO as IO

takeGuess :: Int -> Int -> IO ()
takeGuess solution num = do
  putStr $ "Guess " ++ show num ++ ": "
  IO.hFlush IO.stdout
  guess <- readLn :: IO Int
  guessNumber guess solution num

guessNumber :: Int -> Int -> Int -> IO ()
guessNumber guess solution num
  | guess == solution = putStrLn "Correct! You have won!"
  | num == 5 = putStrLn $ "Haha, I won! The number was " ++ show solution
  | guess > solution = do 
    putStrLn "Too high! Try again!"
    takeGuess solution $ succ num
  | otherwise = do
    putStrLn "Too low! Try again!"
    takeGuess solution $ succ num

main :: IO ()
main = do
  solution <- R.randomRIO(1, 100) :: IO Int
  putStrLn "Guess-a-number game!"
  putStrLn "I am thinking of a number between 1 and 100"
  putStrLn "You have 5 tries to guess it correctly or I win"
  putStrLn "What's your guess?"
  takeGuess solution 1
  
-- TAIL INFO:
-- Name: Guess number
-- Language: Haskell
-- Compile: ghc --make guess_number.hs
-- State: Done
--
-- Play guess-a-number game
--
-- Example: ./guess_number
