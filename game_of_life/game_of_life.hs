import qualified System.Environment as Env
import qualified System.Random as Rand
import qualified Graphics.UI.SDL as SDL

main = do
  
  -- Handle arguments
  argv <- Env.getArgs
  (argW, argH) <- case argv of
    [w, h] -> case ((reads w, reads h) :: ([(Int, String)], [(Int, String)])) of
      ([(ww, _)], [(hh, _)]) -> return (ww, hh)
      _ -> error "Usage: ./game_of_life <width> <height>"
    _ -> error "Usage: ./game_of_life <width> <height>"
  
  putStrLn $ "Width: " ++ show argW ++ ", Height: " ++ show argH
  
  
  --SDL.init [SDL.InitEverything]


-- TAIL INFO:
-- Name: Game of Life
-- Language: Haskell
-- State: Not done
-- Compile: ghc --make game_of_life.hs
--
-- Display randomized game of life
--
-- 
-- Example: ./game_of_life 150 50
--
