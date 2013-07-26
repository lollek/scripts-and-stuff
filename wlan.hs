module Main where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import Data.Text (pack, unpack, splitOn)
import System.Console.ANSI
import Control.Monad (when)
import System.IO (hFlush, stdout)

-- Macro for `iwlist wlan0 scan`
wlanScan :: IO (ExitCode, String, String)
wlanScan = readProcessWithExitCode "iwlist" ["wlan0", "scan"] []

-- Split above result into Array[Cell]
scanToCells :: String -> [String]
scanToCells msg = drop 1 $ map unpack $ splitOn (pack "Cell") (pack msg)

-- Split above result into (mac, quality, enc, essid)
formatCell :: String -> (String, String, String, String)
formatCell cell = do
  let splitCell = lines cell
    in (
    drop 15 $ splitCell !! 0,          -- MAC
    take 2 $ drop 28 $ splitCell !! 3, -- QUALITY
    drop 35 $ splitCell !! 4,          -- ENCRYPTION
    init $ drop 27 $ splitCell !! 5    -- ESSID
    )
  
-- Print a cell
printCell :: (String, String, String, String) -> IO ()
printCell (mac, quality, enc, essid) = do
  if enc == "on" then
    setSGR [SetColor Foreground Vivid Red]
    else
    setSGR [SetColor Foreground Vivid Green]
  putStrLn $ "\t" ++ mac ++ " - " ++ quality ++ "/70 - " ++ enc ++ " - " ++ essid
  setSGR [Reset]
  
printAllCells :: [(String, String, String, String)] -> IO ()
printAllCells cellList = do
  putStrLn $ show (length cellList) ++ " access points found: "
  mapM_ printCell cellList

formatScan :: String -> IO ()
formatScan scanResult = printAllCells $ map formatCell $ scanToCells scanResult
  


main = do
  putStr "Scanning for access points ... "
  hFlush stdout
  
  (code, msg, _) <- wlanScan
  when (code /= ExitSuccess)
    (putStrLn $ "[ERR] - Are you really signed on as root?"); exitFailure
  
  putStrLn "[OK]"
  formatScan msg


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
