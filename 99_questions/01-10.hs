-- http://www.haskell.org/haskellwiki/99_questions/1_to_10

-- Find the last element of a list
myLast :: [a] -> a
myLast ([x]) = x
myLast (_:xs) = myLast xs
myLast _ = error "Did not receive a list"

-- Find the last but one element of a list
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "Did not receive a list"

-- Find the N:th element of a list
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)
elementAt _ _ = error "Index out of bounds"

-- Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Check if palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Flatten nested list
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Remove consecutive duplicates in a list
compress :: Eq a => [a] -> [a]
--compress (x:xs) = x:compress xs