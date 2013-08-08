import qualified System.Environment as Env
import qualified System.Random as Rand
import qualified Graphics.UI.SDL as SDL

initMap :: Rand.RandomGen g => Int -> g -> [Int]
initMap i = take i . Rand.randomRs (0, 1)
  

main = do
  
  -- Handle arguments
  argv <- Env.getArgs
  (argW, argH) <- case argv of
    [w, h] -> case ((reads w, reads h) :: ([(Int, String)], [(Int, String)])) of
      ([(ww, _)], [(hh, _)]) -> return (ww, hh)
      _ -> error "Usage: ./game_of_life <width> <height>"
    _ -> error "Usage: ./game_of_life <width> <height>"
  
  -- Create random number generator:
  stdGen <- Rand.getStdGen
  -- world = initMap (argW * argH) stdGen
  
  
  -- Init SDL
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode argW argH 32 []
  SDL.setCaption "Game of Life" "Game of Life"
  --stdscr <- SDL.getVideoSurface
  
  
  -- SDL.flip stdscr
  SDL.quit

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
