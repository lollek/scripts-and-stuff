module Main where

import System.Environment
import Data.Char

rot13 :: Char -> Char
rot13 c
  | 'a' <= toLower c && toLower c <= 'm' = chr(ord c + 13)
  | 'n' <= toLower c && toLower c <= 'z' = chr(ord c - 13)
  | otherwise = c

main = do
  arg <- System.Environment.getArgs
  if length arg > 0 then
    putStrLn $ map rot13 $ unwords arg
    else
    interact $ map rot13
    
-- TAIL INFO:
-- Name: Rot13
-- Language: Haskell
-- Compile: ghc --make rot13.hs
-- State: Done
--
-- Rotate the argument by 13 letters
--
-- Example: ./rot13 hello world
-- Example2: cat rot13.hs | ./rot13
--
