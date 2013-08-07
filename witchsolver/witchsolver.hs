module Main where

data Card = Card { number, up, left, down, right :: Int }

(p_head, p_tail) = (1, -1)
(r_head, r_tail) = (2, -2)
(y_head, y_tail) = (3, -3)
(g_head, g_tail) = (4, -4)
                            
initDeck = [ Card 1 g_head r_tail p_tail p_head
           , Card 2 g_tail y_tail g_head r_head
           , Card 3 r_tail y_tail g_head y_head
           , Card 4 p_head r_tail p_tail g_head
           , Card 5 g_tail y_tail p_head r_head
           , Card 6 g_tail r_tail p_head y_head
           , Card 7 p_head y_tail r_tail g_head
           , Card 8 p_tail r_tail p_head y_head
           , Card 9 p_tail y_tail g_head r_head
           ]

rotated (Card {number=n, up=u, left=l, down=d, right=r}) = 
      [Card {number=n, up=u, left=l, down=d, right=r}
      ,Card {number=n, up=l, left=d, down=r, right=u}
      ,Card {number=n, up=d, left=r, down=u, right=l}
      ,Card {number=n, up=r, left=u, down=l, right=d}
      ]
  
manageLevel :: Int -> [Card] -> [Card] -> IO ()
manageLevel l deck solution
  | l == 0         = mapM_ (\x -> manageLevel 1 (rm x deck) [x]) $ concat $ map rotated deck
  | l < 3          = checkNext fitsLeft
  | l `elem` [3,6] = checkNext fitsUp
  | l == 9         = printSolution solution
  | otherwise      = checkNext fitsUpLeft
  where 
    fitsUp card = up card + down (solution !! ((-3) + length solution)) == 0
    fitsLeft card = left card + right (last solution) == 0
    fitsUpLeft card = fitsUp card && fitsLeft card
    rm c (x:xs) = if number x == number c then xs else if null xs then [x] else x:rm c xs
    nextlvl x = manageLevel (l+1) (rm x deck) (solution ++ [x])
    checkNext f = mapM_ (\x -> if f x then nextlvl x else return ()) $concat $ map rotated deck

printSolution :: [Card] -> IO ()
printSolution (a:b:c:d:e:f:g:h:i:j) =
  putStr $ unlines $ ["Solution:"
                     ,show (number a) ++ " " ++ show (number b) ++ " " ++ show (number c)
                     ,show (number d) ++ " " ++ show (number e) ++ " " ++ show (number f)
                     ,show (number g) ++ " " ++ show (number h) ++ " " ++ show (number i)
                     ]

main = manageLevel 0 initDeck []
