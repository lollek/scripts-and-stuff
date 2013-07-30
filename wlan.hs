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

printInColor :: String -> Xterm.Color -> IO ()
printInColor string color = do
  Xterm.setSGR [Xterm.SetConsoleIntensity Xterm.BoldIntensity]
  Xterm.setSGR [Xterm.SetColor Xterm.Foreground Xterm.Vivid color]
  putStrLn string
  Xterm.setSGR [Xterm.Reset]

raiseHaikuError :: IO ()
raiseHaikuError = do
  printInColor "Error" Xterm.Red
  putStrLn $ unlines [
    "", "Errors have occured",
    "We won't tell you where of why",
    "Lazy programmers"]
  Exit.exitFailure

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
  putStrLn $ show (length cellList) ++ " access points found \n"
  printInColor " No - Signal - Encrypt\t- Name" Xterm.White
  mapM_ printCell cellList
  putStrLn ""
  where 
    printCell (num, quality, enc, essid) = do
      case enc of
        "0" -> printInColor (num ++ " -   " ++ quality ++ "%  -   on\t- " ++ essid) Xterm.Red
        "1" -> printInColor (num ++ " -   " ++ quality ++ "%  -   off\t- " ++ essid) Xterm.Blue
        _ -> printInColor (num ++ " -   " ++ quality ++ "%  -  known\t- " ++ essid) Xterm.Green
  
connectTo :: (String, String, String, String) -> IO ()  
connectTo (num, quality, "2", essid) = do
  -- Configure wpa
  putStrLn "Configuring wpa ... "
  (wpaCode, _, _) <- SP.readProcessWithExitCode "wpa_supplicant"
         ["-B", "-i", "wlan0", "-c", "/root/wlan/" ++ essid] []
  case wpaCode of
    Exit.ExitFailure _ -> raiseHaikuError
    _ -> printInColor "OK" Xterm.Green
      
  -- Run dhclient
  putStrLn "Asking kindly for an IP address ... "
  (dhCode, _, _) <-SP.readProcessWithExitCode "dhclient" ["wlan0"] []
  case dhCode of 
    Exit.ExitFailure _ -> raiseHaikuError
    _ -> do
      printInColor "OK" Xterm.Green
      Exit.exitSuccess
connectTo _ = raiseHaikuError      
  
main = do
  
  -- Make a list of known access points
  putStr "Checking for known access points ... "
  IO.hFlush IO.stdout
  
  dirScan <- E.try (Dir.getDirectoryContents "/root/wlan") :: IO (Either IOError [String])
  case dirScan of
    Left err -> do 
      printInColor ("Error - " ++ IOE.ioeGetErrorString err) Xterm.Red
      Exit.exitFailure
    Right knownAccessPoints -> do 
      printInColor "OK" Xterm.Green
  
      -- Try to check for nearby access points
      putStr "Scanning for access points ... "
      IO.hFlush IO.stdout

      (exitCode, stringOfCells, _) <- SP.readProcessWithExitCode "iwlist" ["wlan0", "scan"] []
      case exitCode of
        Exit.ExitFailure _ -> do
          raiseHaikuError
        _ -> do
          printInColor "OK" Xterm.Green
          printCells knownCells
          putStrLn $ "Select WLAN to connect to (1-" ++ 
            (show (length knownCells)) ++ ") or ^C to cancel"
          selectedCell <- readLn :: IO Int
          connectTo (knownCells !! pred selectedCell)
          where
            knownAccessPoints' = drop 2 $ L.sort knownAccessPoints 
            knownCells = formatCells knownAccessPoints' stringOfCells
              
                    
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
