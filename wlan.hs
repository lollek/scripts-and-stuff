module Main where

import qualified Control.Exception as E
import qualified Data.Either as Either
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified System.Console.ANSI as Xterm
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Error as IOE
import qualified System.Posix.Unistd as Unistd
import qualified System.Process as SP

printInColor :: String -> Xterm.Color -> IO ()
printInColor string color = do
  Xterm.setSGR [Xterm.SetConsoleIntensity Xterm.BoldIntensity]
  Xterm.setSGR [Xterm.SetColor Xterm.Foreground Xterm.Vivid color]
  putStrLn string
  Xterm.setSGR [Xterm.Reset]

-- Raise general error
raiseHaikuError :: IO ()
raiseHaikuError = do
  printInColor "Error" Xterm.Red
  putStrLn $ unlines [
    "", "Errors have occured",
    "We won't tell you where of why",
    "Lazy programmers"]
  Exit.exitFailure

-- Formats result from wlan0 scan
formatCells :: [String] -> String -> [(String, String, String)]
formatCells accessPoints = 
  map (\c -> (quality c, enc c, essid c)) . drop 1 . LS.splitOn "Cell"
  where
    essid = init . drop 27 . (!!5) . lines
    enc c
      | essid c `elem` accessPoints = "2"
      | drop 35 ((lines c) !! 4) == "off" = "1"
      | otherwise = "0"
    quality = show . floor . (/70) . (*100) . read . take 2 . drop 28 . (!!3) . lines
        
-- Compare cells by 1: encrypt / 2: quality        
compareCells :: Ord a => (a, a, a) -> (a, a, a) -> Ordering
compareCells (q1, e1, _) (q2, e2, _)
  | e1 < e2 = GT
  | e1 > e2 = LT
  | q1 < q2 = GT
  | q1 > q2 = LT
  | otherwise = EQ

-- Prints the cells from FormatCells
printCells :: [(String, String, String)] -> IO ()
printCells cellList = do
  putStrLn $ show (length cellList) ++ " access points found \n"
  printInColor " No - Signal - Encrypt\t- Name" Xterm.White
  mapM_ printCell $ zip [1..] cellList
  putStrLn ""
  where 
    printCell (num, (quality, enc, essid)) = do
      case enc of
        "0" -> printInColor (no ++ " -   " ++ quality ++ "%  -   on\t- " ++ essid) Xterm.Red
        "1" -> printInColor (no ++ " -   " ++ quality ++ "%  -   off\t- " ++ essid) Xterm.Blue
        _ -> printInColor (no ++ " -   " ++ quality ++ "%  -  known\t- " ++ essid) Xterm.Green
      where no = if (length (show num) == 1)
                 then "0" ++ show num
                 else show num
  
-- Connects to access point
wpaSupplicant :: (String, String, String) -> IO ()
-- Unknown encrypted
wpaSupplicant (_, "0", essid) = do
  putStrLn $ "Attempting to connect to " ++ essid
  putStr "Password: "
  IO.hFlush IO.stdout
  
  passwd <- getLine --FIX: Can fail
  putStr "Generating WPA PSK ... "
  IO.hFlush IO.stdout
  (passCode, passStdOut, _) <- SP.readProcessWithExitCode "wpa_passphrase" 
                               [essid, passwd] []
  case passCode of
    Exit.ExitFailure _ -> raiseHaikuError
    _ -> return ()
  
  (path, handle) <- IO.openTempFile "/tmp" ("wlan.hs."++essid) --FIX: Can fail
  printInColor ("OK: "++path) Xterm.Green
  IO.hPutStrLn handle passStdOut
  IO.hClose handle
  putStr "Configuring connection ... "
  IO.hFlush IO.stdout
  
  (wpaCode, _, _) <- SP.readProcessWithExitCode "/sbin/wpa_supplicant"
                     (words ("-B -i wlan0 -D nl80211 -c "++path)) []
  case wpaCode of
    Exit.ExitFailure _ -> raiseHaikuError
    _ -> printInColor "OK" Xterm.Green
    
  dhClient
    -- FIX: save psk file to /root/wlan here
    
-- Known / unencrypted    
wpaSupplicant (_, enc, essid) = do
  putStrLn $ "Attempting to connect to " ++ essid
  putStr "Configuring connection ... "
  IO.hFlush IO.stdout
  
  (wpaCode, _, _) <- 
    case enc of
      "2" -> SP.readProcessWithExitCode "/sbin/wpa_supplicant"
             (words ("-B -i wlan0 -D nl80211 -c /root/wlan/" ++ essid)) []
      _ -> SP.readProcessWithExitCode "/sbin/iwconfig" ["wlan0", "essid", essid] []
      
  case wpaCode of
    Exit.ExitFailure _ -> raiseHaikuError
    _ -> printInColor "OK" Xterm.Green
  
  dhClient
        
-- Start dhcp service
dhClient :: IO ()      
dhClient = do
  putStr "Waiting for an IP address ... "
  IO.hFlush IO.stdout
  (dhCode, _, _) <- SP.readProcessWithExitCode "/sbin/dhclient" ["wlan0", "-1"] []
  case dhCode of 
    Exit.ExitFailure _ -> do -- FIX: Kill wpa_supplicant here
      printInColor "Error - No IP received" Xterm.Red
      Exit.exitFailure
    _ -> printInColor "OK" Xterm.Green

-- Kill processes
wlanKill :: IO ()
wlanKill = do
  let processes = ["dhclient", "wpa_supplicant"] in do
    putStr $ ("Killing " ++ (unwords (L.intersperse "/" processes)) ++ " ... ")
    IO.hFlush IO.stdout
    (killCode, _, killStdErr) <- SP.readProcessWithExitCode "killall" processes []
  
    case killCode of
      Exit.ExitFailure _ -> printInColor ("Error:\n" ++ killStdErr) Xterm.Red
      _ -> printInColor "OK" Xterm.Green
  
  putStr "Setting wlan0 to up ... "
  IO.hFlush IO.stdout
  Unistd.sleep 1
  (ifCode, _, _) <- SP.readProcessWithExitCode "/sbin/ifconfig" (words "wlan0 up") []
  case ifCode of
    Exit.ExitFailure _ -> printInColor "Failed" Xterm.Red
    _ -> printInColor "OK" Xterm.Green
    
  Exit.exitSuccess

connect :: String -> IO ()
connect mode = do
  -- List known access points
  putStr "Checking for known access points ... "
  IO.hFlush IO.stdout
  dirScan <- E.try (Dir.getDirectoryContents "/root/wlan") :: IO (Either IOError [String])
  case dirScan of
    Left err -> do
      printInColor ("Error - /root/wlan " ++ IOE.ioeGetErrorString err) Xterm.Red
      Exit.exitFailure
    Right _ -> printInColor "OK" Xterm.Green  
      
  -- Try to check for nearby access points
  putStr "Scanning for access points ... "
  IO.hFlush IO.stdout
  (exitCode, stringOfCells, _) <- SP.readProcessWithExitCode "/sbin/iwlist" ["wlan0", "scan"][]
  case exitCode of
    Exit.ExitFailure _ -> raiseHaikuError
    _ -> printInColor "OK" Xterm.Green
    
  -- Print them out
  let knownCells = 
        L.sortBy compareCells $ flip (formatCells) stringOfCells $
        drop 2 $ L.sort $ concat $ Either.rights [dirScan]
  printCells knownCells
  
  -- Select manually or automatically (automatically will only connect to known networks)
  case mode of
    "auto" -> do
      let (q, c, e) = (knownCells !! 0) in
        if c == "2" 
        then wpaSupplicant (knownCells !! 0)
        else putStrLn "No known access point found"
    _ -> do
      putStrLn $ "Select WLAN to connect to (1-" ++ 
        (show (length knownCells)) ++ ") or ^C to cancel"
      selectedCell <- readLn :: IO Int --FIX: can fail
      if (1 <= selectedCell && selectedCell <= length knownCells) 
        then wpaSupplicant (knownCells !! pred selectedCell)
        else putStrLn $ "Seriously? Do you see a "++(show selectedCell)++" in the list?"

main = do
  -- Print header
  Xterm.setSGR [Xterm.SetUnderlining Xterm.SingleUnderline]
  putStrLn "Lollian WLAN Helper - 2013-08-02\n"
  Xterm.setSGR [Xterm.Reset]
  
  -- Check for argvs
  argv <- Env.getArgs
  case unwords argv of
    "auto" -> connect "auto"
    "man" -> connect "man"
    "kill" -> wlanKill
    _ -> putStrLn $ unlines
         ["Usage: wlan <mode>"
         ,"Modes:"
         ," auto   - automatically connect to best access point"
         ," man    - manually select an access point"
         ," kill   - kill wpa_supplicant and dhclient"
         ]


              
                    
-- TAIL INFO:
-- Name: Lollian Wlan Scan and Connect
-- Language: Haskell
-- Compile: ghc --make wlan.hs
-- State: Done w/ bugs
--
-- Find and connect to wireless access points
-- WPA Passwords should also be stored under /root/wlan
--
-- Example: ./wlan
--
