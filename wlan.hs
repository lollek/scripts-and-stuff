module Main where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import Data.Text (pack, unpack, splitOn)
import System.Console.ANSI
import Control.Monad (when)
import System.IO (hFlush, stdout)

-- (ExitCode, stdout, stderr) for `iwlist wlan0 scan`
wlanScan :: IO (ExitCode, String, String)
wlanScan = readProcessWithExitCode "iwlist" ["wlan0", "scan"] []

-- Takes a string, splits it by mention of "Cell"
--  and returns all except the first instances as an array
scanToCells :: String -> [String]
scanToCells msg = drop 1 $ map unpack $ splitOn (pack "Cell") (pack msg)

-- Takes one of the Cells above and splits it into a tuple of
--  (mac, quality, encryption, essid)
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
  
-- Prints several cells through above function
printCells :: [(String, String, String, String)] -> IO ()
printCells cellList = do
  putStrLn $ show (length cellList) ++ " access points found: "
  mapM_ printCell cellList

-- Takes stdout from `iwlist wlan0 scan` and prints all cells
formatScan :: String -> IO ()
formatScan scanResult = printCells $ map formatCell $ scanToCells scanResult
  


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
