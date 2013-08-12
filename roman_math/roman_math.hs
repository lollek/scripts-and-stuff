module Main where

import qualified System.Environment as Env
import qualified Data.Char as Char

romeFromInt :: Int -> String
romeFromInt c
  | c >= 1000 = 'M'    : romeFromInt(c - 1000)
  | c >= 900  = 'C':'M': romeFromInt(c - 900)
  | c >= 500  = 'D'    : romeFromInt(c - 500)
  | c >= 400  = 'C':'D': romeFromInt(c - 400)
  | c >= 100  = 'C'    : romeFromInt(c - 100)
  | c >= 90   = 'X':'C': romeFromInt(c - 90)
  | c >= 50   = 'L'    : romeFromInt(c - 50)
  | c >= 40   = 'X':'L': romeFromInt(c - 40)
  | c >= 10   = 'X'    : romeFromInt(c - 10)
  | c >= 9    = 'I':'X': romeFromInt(c - 9)
  | c >= 5    = 'V'    : romeFromInt(c - 5)
  | c >= 4    = 'I':'V': romeFromInt(c - 4)
  | c >= 1    = 'I'    : romeFromInt(c - 1)
  | otherwise = []
                
intFromRome :: String -> Int
intFromRome (x:s:xs)
  | x == 'C' && s == 'M' = 900 + intFromRome xs
  | x == 'C' && s == 'D' = 400 + intFromRome xs
  | x == 'X' && s == 'C' = 90 + intFromRome xs
  | x == 'X' && s == 'L' = 40 + intFromRome xs
  | x == 'I' && s == 'X' = 9 + intFromRome xs
  | x == 'I' && s == 'V' = 4 + intFromRome xs
intFromRome (x:xs)
  | x == 'M'  = 1000 + intFromRome xs
  | x == 'D'  = 500 + intFromRome xs
  | x == 'C'  = 100 + intFromRome xs
  | x == 'L'  = 50 + intFromRome xs
  | x == 'X'  = 10 + intFromRome xs
  | x == 'V'  = 5 + intFromRome xs
  | x == 'I'  = 1 + intFromRome xs
  | otherwise = intFromRome xs
intFromRome "" = 0

convert :: String -> IO ()
convert num = do
  if all Char.isDigit num 
    then putStrLn $ romeFromInt (read num :: Int)
    else if all Char.isAlpha num 
         then print $ intFromRome num
         else putStrLn "Unknown"

main = do 
  argv <- Env.getArgs
  if (length argv > 0) 
    then convert (argv !! 0)
    else putStrLn $ unlines 
         ["Example usages:"
         ,"\t./roman_math XVI"
         ,"\t./roman_math 15"
         ]




-- TAIL INFO:
-- Name: Roman Math
-- Language: Haskell
-- Compile: ghc --make roman_math.hs
-- State: Done
--
-- Convert a number from decimal to roman or vice versa
--
-- Example: ./roman_math 42
-- Example2: ./roman_math 1990
--
