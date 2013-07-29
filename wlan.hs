module Main where

import qualified Data.List.Split as LS
import qualified System.Console.ANSI as Xterm
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Error as IOE
import qualified System.Process as SP

-- Formats result from wlan0 scan
formatCells :: String -> [(String, String, String, String)]
formatCells = map formatCell . drop 1 . LS.splitOn "Cell"
  where 
    formatCell c = (num c, quality c, enc c, essid c)
      where
        num = take 3 . (!! 0). lines
        quality = toPercent . take 2 . drop 28 . (!! 3) .lines
          where toPercent = show . floor . (/70) . (*100) . read
        enc = drop 35 . (!! 4) . lines
        essid = init .drop 27 . (!! 5) . lines

-- Prints the cells from FormatCells
printCells :: [(String, String, String, String)] -> IO ()
printCells cellList = do
  putStrLn $ show (length cellList) ++ " access points found: "
  putStrLn " No - Signal - Enc\t- Name"
  mapM_ printCell cellList
  where 
    printCell (num, quality, enc, essid) = do
      case enc of
        "on" -> Xterm.setSGR [Xterm.SetColor Xterm.Foreground Xterm.Vivid Xterm.Red]
        _ -> Xterm.setSGR [Xterm.SetColor Xterm.Foreground Xterm.Vivid Xterm.Blue]
      putStrLn $ num ++ " - " ++ quality ++ "%    - " ++ enc ++ "\t- " ++ essid
      Xterm.setSGR [Xterm.Reset]

          
main = do
  
  -- Try to check for nearby access points
  putStr "Scanning for access points ... "
  IO.hFlush IO.stdout

  (code, msg, _) <- SP.readProcessWithExitCode "iwlist" ["wlan0", "scan"] []
  case code of
    Exit.ExitFailure _ -> do
      putStrLn "[ERR]\nAre you really signed on as root?"
      Exit.exitFailure
    _ -> do
      putStrLn "[OK]"
      
  printCells $ formatCells msg

  -- Check for known access points
  --putStr "Checking for known access points ... "
  --wpaList <- listWpas `E.catch` handleError
  --where
    --listWpas = Dir.getDirectoryContents "/root/wlan"
    --handleError e = putStrLn $ unlines [ 
    --"[Err]",
    --IOE.ioeGetErrorString e ]
    
  --wpaList <- Dir.getDirectoryContents "/root/wlan"
  --drop 2 $ List.sort wpaList
  --print wpaList


-- TAIL INFO:
-- Name: Lollian Wlan Scan and Connect
-- Language: Haskell
-- Compile: ghc --make wlan.hs
-- State: Not done
--
-- Find and connect to wireless access points
-- WPA Passwords should also be stored under $HOME/.wlan
--
-- Example: ./wlan auto
--
