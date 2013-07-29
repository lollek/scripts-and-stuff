module Lollian (split) where

-- Lollian.split "e" "hello" == ["h", "llo"]
split :: String -> String -> [String]
split _ [] = [""]
split w (x:xs)
  | take (length w) (x:xs) == w = "" : split w (drop (length w) (x:xs))
  | otherwise = (x: head (split w xs)) : tail (split w xs)
 
-- TAIL INFO:
-- Name: Lollian Haskell Functions
-- Language: Haskell
-- State: Done
--
-- Will put stuff here if I dunno where to place them
-- 
--
-- 
-- 
-- 






