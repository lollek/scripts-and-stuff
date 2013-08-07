-- http://www.haskell.org/haskellwiki/99_questions/21_to_30
import qualified System.Random as Rand
import qualified Data.List as List

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
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  stdGen <- Rand.getStdGen
  return $ rnd_select_ xs n stdGen
    where
      rnd_select_ _ 0 _ = []
      rnd_select_ xs n gen = (xs !! rndI) : rnd_select_ (rest xs rndI) (n-1) newGen
        where 
          (rndI, newGen) = Rand.randomR(0, length xs - 1) gen
          rest (x:xs) n = if n > 0 then x:rest xs (n-1) else xs

-- Lotto: Draw N different numbers from the set 1..M
diff_select :: Int -> Int -> IO [Int]
diff_select n top = do
  stdGen <- Rand.getStdGen
  return $ take n $ List.nub $ [[1..top] !! i | i <- Rand.randomRs(0, top -1) stdGen]
          
-- Generate a random permutation of the elements of a list          
rnd_permu :: Eq a => [a] -> IO [a]
rnd_permu xs = do
  stdGen <- Rand.getStdGen
  return $ rnd_permu' xs stdGen
    where 
      rnd_permu' [] _ = []
      rnd_permu' xs gen = (xs !! randI) : rnd_permu' rest newGen
        where
          (randI, newGen) = Rand.randomR(0, length xs -1) gen
          rest = take randI xs ++ drop (randI+1) xs
  
-- Generate the combinations of K distinct objects from the N elements of a list          
-- Not sure how to solve this yet..
          
          
