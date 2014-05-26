module Main where

import System.Environment ( getArgs )
import Data.Char ( isDigit, isAlpha )

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
intFromRome ('M':xs)     = 1000 + intFromRome xs
intFromRome ('D':xs)     = 500 + intFromRome xs
intFromRome ('C':'M':xs) = 900 + intFromRome xs
intFromRome ('C':'D':xs) = 400 + intFromRome xs
intFromRome ('C':xs)     = 100 + intFromRome xs
intFromRome ('L':xs)     = 50 + intFromRome xs
intFromRome ('X':'C':xs) = 90 + intFromRome xs
intFromRome ('X':'L':xs) = 40 + intFromRome xs
intFromRome ('X':xs)     = 10 + intFromRome xs
intFromRome ('V':xs)     = 5 + intFromRome xs
intFromRome ('I':'X':xs) = 9 + intFromRome xs
intFromRome ('I':'V':xs) = 4 + intFromRome xs
intFromRome ('I':xs)     = 1 + intFromRome xs
-- Normally, silent failure would be bad, but in this case it's OK:
intFromRome (_:xs)       = intFromRome xs
intFromRome []           = 0

convert :: [String] -> IO ()
convert [] = 
  putStrLn $ unlines
    ["Example usages:"
    ,"\t./roman_math XVI"
    ,"\t./roman_math 15"
    ]
convert (num:_)
  | all isDigit num = putStrLn $ romeFromInt (read num :: Int)
  | all isAlpha num = print $ intFromRome num
  | otherwise = putStrLn "Received data was neither roman nor decimal"

main :: IO ()
main = getArgs >>= convert

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
