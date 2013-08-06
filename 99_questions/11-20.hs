-- http://www.haskell.org/haskellwiki/99_questions/11_to_20


data EncodedResult a = Single a | Multiple Int a
                     deriving (Show)
                              
-- Modified run-length encoding
encodeModified :: Eq a => [a] -> [EncodedResult a]
encodeModified [] = []
encodeModified xs@(x:xs')
  | encodeLen == 1 = Single x: encodeModified xs'
  | otherwise      = Multiple encodeLen x: encodeModified (dropWhile (==x) xs')
    where encodeLen = length $ takeWhile (==x) xs
          
-- Decode a run-length encoded list
decodeModified :: [EncodedResult a] -> [a]
decodeModified [] = []
decodeModified (Single val:xs) = val:decodeModified xs
decodeModified (Multiple no val:xs) = replicate no val ++ decodeModified xs

-- Run-length encoding of a list (direct solution)
-- I do not understand the question...

-- Duplicate elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x: dupli xs

-- Replicate elements of a list N times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = mrepli x n ++ repli xs n
  where 
    mrepli :: a -> Int -> [a]
    mrepli _ 0 = []
    mrepli x n = x :mrepli x (n-1)
    
-- Drop every N:th element from a list    
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

-- Split a list into two parts - the length of first part is given
split :: [a] -> Int -> ([a], [a])
split xs n = (part1 xs n, part2 xs n)
  where 
    part1 xs 0 = []
    part1 (x:xs) n = x:part1 xs (n-1)
    part2 xs 0 = xs
    part2 (x:xs) n = part2 xs (n-1)
    
-- Extract a slice from a list    
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs nf nt = take (nt -nf +1) $ drop (nf -1) xs

-- Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
  | n > length xs     = rotate xs (n - length xs)
  | n >= 0            = drop n xs ++ take n xs
  | otherwise         = rotate xs (n + length xs)

-- Remove the N:th element from a list
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), restOfList n xs)
  where 
    restOfList 1 (x:xs) = xs
    restOfList n (x:xs) = x: restOfList (n-1) xs
    
