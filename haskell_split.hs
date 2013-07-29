split :: String -> String -> [String]
split _ [] = [""]
split w (x:xs)
  | take (length w) (x:xs) == w = "" : split w (drop (length w) (x:xs))
  | otherwise = (x: head (split w xs)) : tail (split w xs)
 
-- TAIL INFO:
-- Name: Split
-- Language: Haskell
-- State: Done
--
-- split "e" "hello" == ["h","llo"]
-- did this to replace splitOn from Data.Text but later changed my mind
--
-- This is not a script but a helping module
-- 
-- 
