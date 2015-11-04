
merge :: [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge l1 l2 = merge' l1 (length l1) l2 (length l2)
  where
    merge' :: [a] -> Int -> [a] -> Int -> [a]
    merge' l1 sz1 l2 sz2
      | sz2 > sz1 = merge' l2 sz2 l1 sz1
      | merge'' l1 l2 interval 0
        where
        interval = if (sz1 `mod` sz2 /= 0)
          then sz1 `div` sz2
          else (sz1 `div` sz2) (-1)
        merge'' :: [a] -> [a] -> Int -> Int -> [a]
        merge'' l [] _ _ = l
        merge'' (x:xs) s interval current
          | current < interval = x : merge'' xs s interval (succ current)
        merge'' l (x:xs) interval _ = x : merge'' l xs interval 0

main :: IO ()
main = do
  putStrLn "Expected: [1,0,2,0,3,0,4]"
  print $ merge [1, 2, 3, 4] [0, 0, 0]
  putStrLn "Expected: [1,2,0,3,4,0,5]"
  print $ merge [1, 2, 3, 4, 5] [0, 0]
  putStrLn "Expected: [1,0,2,0,3,0,4,5]"
  print $ merge [0, 0, 0] [1, 2, 3, 4, 5]
  putStrLn "Expected: ['1','2','0','3','4','0','5','6']"
  print $ merge ['0', '0'] ['1', '2', '3', '4', '5', '6']
  putStrLn "Expected: [1,0,2,0,3,0,4,0,5,0,6,7,8]"
  print $ merge [0, 0, 0, 0, 0] [1, 2, 3, 4, 5, 6, 7, 8]
