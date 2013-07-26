module Main where

import qualified Data.Text as Text
import qualified System.Console.ANSI as Xterm
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Process as S

-- (ExitCode, stdout, stderr) for `iwlist wlan0 scan`
wlanScan :: IO (Exit.ExitCode, String, String)
wlanScan = SP.readProcessWithExitCode "iwlist" ["wlan0", "scan"] []

--dirScan :: IO (Exit.ExitCode, [String])
--dirScan = 

-- Takes a string (stdout from wlanScan) and splits it by mention of "Cell"
-- then takes each Cell and splits it into a tuple and returns
-- [(num, quality, encryption, essid)]
formatCells :: String -> [(String, String, String, String)]
formatCells string = map formatCell newString
  where 
    -- Split by cell and drop the first instance:
    newString = drop 1 $ map Text.unpack $ Text.splitOn (Text.pack "Cell") (Text.pack string)
    -- Format to (num, quality, encryption, essid):
    formatCell cells =
      ( take 3 $ cell !! 0,                        -- NUM
        toPercent $ take 2 $ drop 28 $ cell !! 3,  -- QUALITY
        drop 35 $ cell !! 4,                       -- ENCRYPTION
        init $ drop 27 $ cell !! 5 )               -- ESSID
      where 
        -- Split to array: 
        cell = lines cells
        -- Change from e.g 50 (max is 70) to 71 (max is 100)
        toPercent x = show $ floor $ (read x :: Double) / 70 * 100
    
-- Prints the cells from FormatCells
printCells :: [(String, String, String, String)] -> IO ()
printCells cellList = do
  putStrLn $ show (length cellList) ++ " access points found: "
  putStrLn " No - Signal - Enc\t- Name"
  mapM_ printCell cellList
  where 
    -- Print a cell:
    printCell (num, quality, enc, essid) = do
      if enc == "on" then
        Xterm.setSGR [Xterm.SetColor Xterm.Foreground Xterm.Vivid Xterm.Red]
        else
        Xterm.setSGR [Xterm.SetColor Xterm.Foreground Xterm.Vivid Xterm.Blue]
      putStrLn $ num ++ " - " ++ quality ++ "%    - " ++ enc ++ "\t- " ++ essid
      Xterm.setSGR [Xterm.Reset]

          
main = do
  putStr "Scanning for access points ... "
  IO.hFlush IO.stdout
  
  (code, msg, _) <- wlanScan
  case code of
    Exit.ExitFailure _ -> do
      putStrLn "[ERR] - Are you really signed on as root?"
      Exit.exitFailure
    _ -> do
      putStrLn "[OK]"
      printCells $ formatCells msg

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
