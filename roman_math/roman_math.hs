module Main where

import System.Environment
import Data.Char

romeFromInt :: Int -> String
romeFromInt c
  | c >= 1000 = "M"  ++ romeFromInt(c - 1000)
  | c >= 900  = "CM" ++ romeFromInt(c - 900)
  | c >= 500  = "D"  ++ romeFromInt(c - 500)
  | c >= 400  = "CD" ++ romeFromInt(c - 400)
  | c >= 100  = "C"  ++ romeFromInt(c - 100)
  | c >= 90   = "XC" ++ romeFromInt(c - 90)
  | c >= 50   = "L"  ++ romeFromInt(c - 50)
  | c >= 40   = "XL" ++ romeFromInt(c - 40)
  | c >= 10   = "X"  ++ romeFromInt(c - 10)
  | c >= 9    = "IX" ++ romeFromInt(c - 9)
  | c >= 5    = "V"  ++ romeFromInt(c - 5)
  | c >= 4    = "IV" ++ romeFromInt(c - 4)
  | c >= 1    = "I"  ++ romeFromInt(c - 1)
  | otherwise = ""
                
intFromRome :: String -> Int       
intFromRome ""          = 0
intFromRome "M"         = 1000
intFromRome "D"         = 500
intFromRome "C"         = 100
intFromRome "L"         = 50
intFromRome "X"         = 10
intFromRome "V"         = 5
intFromRome "I"         = 1
intFromRome ('M':t)     = 1000 + intFromRome t
intFromRome ('D':t)     = 500 + intFromRome t
intFromRome ('C':'M':t) = 900 + intFromRome t
intFromRome ('C':'D':t) = 400 + intFromRome t
intFromRome ('C':t)     = 100 + intFromRome t
intFromRome ('L':t)     = 50 + intFromRome t
intFromRome ('X':'C':t) = 90 + intFromRome t
intFromRome ('X':'L':t) = 40 + intFromRome t
intFromRome ('X':t)     = 10 + intFromRome t
intFromRome ('V':t)     = 5 + intFromRome t
intFromRome ('I':'X':t) = 9 + intFromRome t
intFromRome ('I':'V':t) = 4 + intFromRome t
intFromRome ('I':t)     = 1 + intFromRome t
intFromRome (h:t)       = intFromRome t

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
