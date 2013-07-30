module Main where

import qualified Control.Exception as E
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified System.Console.ANSI as Xterm
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Error as IOE
import qualified System.Process as SP

-- Formats result from wlan0 scan
-- Should I rewrite this nasty piece of code?
formatCells :: [String] -> String -> [(String, String, String, String)]
formatCells accessPoints = map formatCell . drop 1 . LS.splitOn "Cell"
  where 
    formatCell c = (num c, quality c, enc c, essid c)
      where
        num = take 3 . (!!0) . lines
        quality = toPercent . take 2 . drop 28 . (!!3) . lines
          where toPercent = show . floor . (/70) . (*100) . read
        enc c
          | init (drop 27 ((lines c) !! 5)) `elem` accessPoints = "2"
          | drop 35 ((lines c) !! 4) == "off" = "1"
          | otherwise = "0"
        essid = init . drop 27 . (!!5) . lines


-- Prints the cells from FormatCells
printCells :: [(String, String, String, String)] -> IO ()
printCells cellList = do
  putStrLn $ show (length cellList) ++ " access points found: "
  putStrLn " No - Signal - Encrypt\t- Name"
  mapM_ printCell cellList
  where 
    printCell (num, quality, enc, essid) = do
      case enc of
        "0" -> do
          setXtermColor Xterm.Red
          putStrLn $ num ++ " - " ++ quality ++ "%    - on\t- " ++ essid
        "1" -> do
          setXtermColor Xterm.Blue
          putStrLn $ num ++ " - " ++ quality ++ "%    - off\t- " ++ essid
        _ -> do
          setXtermColor Xterm.Green
          putStrLn $ num ++ " - " ++ quality ++ "%    - known\t- " ++ essid
      Xterm.setSGR [Xterm.Reset]
      
      where setXtermColor color = do
              Xterm.setSGR [Xterm.SetColor Xterm.Foreground Xterm.Vivid color]
              Xterm.setSGR [Xterm.SetConsoleIntensity Xterm.BoldIntensity]

  
main = do
  
  -- Make a list of known access points
  putStr "Checking for known access points ... "
  IO.hFlush IO.stdout
  
  dirScan <- E.try (Dir.getDirectoryContents "/root/wlan") :: IO (Either IOError [String])
  case dirScan of
    
    Left err -> do 
      putStrLn $ "[Err]\nError message: " ++ IOE.ioeGetErrorString err
      Exit.exitFailure
      
    Right knownAccessPoints -> do 
      putStrLn "[OK]"
  
      -- Try to check for nearby access points
      putStr "Scanning for access points ... "
      IO.hFlush IO.stdout

      (exitCode, stringOfCells, _) <- SP.readProcessWithExitCode "iwlist" ["wlan0", "scan"] []
      case exitCode of
        
        Exit.ExitFailure _ -> do
          putStrLn "[ERR]\nAre you really signed on as root?"
          Exit.exitFailure
          
        _ -> do
          putStrLn "[OK]"
          printCells $ formatCells knownAccessPoints' stringOfCells
          where knownAccessPoints' = drop 2 $ L.sort knownAccessPoints

    
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
