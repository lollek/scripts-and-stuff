-- http://www.haskell.org/haskellwiki/99_questions/21_to_30
import qualified System.Random as Rand

-- Insert element at given position in a list
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs 1 = y:xs
insertAt y (x:xs) n = x: insertAt y xs (n-1)

-- Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range x y
  | y > x     = x:range (x+1) y
  | y < x     = x:range (x-1) y
  | otherwise = [x]
                             
-- Extract a given number of randomly selected elements from a list
--rnd_select :: [a] -> Int -> [a]
--rnd_select xs n = do
--  stdGen <- Rand.getStdGen
--  rnd_select_ xs n stdGen
--    where
--      rnd_select_ xs n gen = (xs !! rndI) : rnd_select_ (rest xs rndI) (n-1) newGen
--        where 
--          (rndI, newGen) = Rand.randomR(1, length xs - 1) gen
--          rest (x:xs) 0 = xs
--          rest (x:xs) n = x:rest xs (n-1)
-- Not sure why this does not work

