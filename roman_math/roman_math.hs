module Main where

import System.Environment
import Data.Char

romeFromInt :: Int -> String
romeFromInt c
  | c >= 1000 = "M" ++ romeFromInt(c - 1000)
  | c >= 900 = "CM" ++ romeFromInt(c - 900)
  | c >= 500 = "D" ++ romeFromInt(c - 500)
  | c >= 400 = "CD" ++ romeFromInt(c - 400)
  | c >= 100 = "C" ++ romeFromInt(c - 100)
  | c >= 90 = "XC" ++ romeFromInt(c - 90)
  | c >= 50 = "L" ++ romeFromInt(c - 50)
  | c >= 40 = "XL" ++ romeFromInt(c - 40)
  | c >= 10 = "X" ++ romeFromInt(c - 10)
  | c >= 9 = "IX" ++ romeFromInt(c - 9)
  | c >= 5 = "V" ++ romeFromInt(c - 5)
  | c >= 4 = "IV" ++ romeFromInt(c - 4)
  | c >= 1 = "I" ++ romeFromInt(c - 1)
  | otherwise = ""
                
intFromRome :: String -> Int       
intFromRome "" = 0
intFromRome (h:"")
  | h == 'M' = 1000
  | h == 'D' = 500
  | h == 'C' = 100
  | h == 'L' = 50
  | h == 'X' = 10
  | h == 'V' = 5
  | h == 'I' = 1
intFromRome ('M':t) = 1000 + intFromRome t
intFromRome ('D':t) = 500 + intFromRome t
intFromRome ('C':h:t)
  | h == 'M' = 900 + intFromRome t
  | h == 'D' = 400 + intFromRome t
  | otherwise = 100 + intFromRome (h:t)
intFromRome ('L':t) = 50 + intFromRome t
intFromRome ('X':h:t) 
  | h == 'C' = 90 + intFromRome t
  | h == 'L' = 40 + intFromRome t
  | otherwise = 10 + intFromRome (h:t)
intFromRome ('V':t) = 5 + intFromRome t
intFromRome ('I':h:t) 
  | h == 'X' = 9 + intFromRome t
  | h == 'V' = 4 + intFromRome t
  | otherwise = 1 + intFromRome (h:t)
intFromRome (h:t) = intFromRome t

usage :: IO ()
usage = putStrLn $ unlines [
  "Example usages:",
  "\t./roman_math XVI",
  "\t./roman_math 15"]

convert :: String -> IO ()
convert num = do
  if all isDigit num then
    putStrLn $ romeFromInt (read num :: Int)
    else if all isAlpha num then
           print $ intFromRome num
         else
           putStrLn "Unknown"
           
  

main = do 
  argv <- getArgs
  if (length argv > 0) then
    convert (argv !! 0)
    else
    usage


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
