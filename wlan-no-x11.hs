module Main where

import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified System.Console.ANSI as Xterm
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Posix.Unistd as Unistd
import qualified System.Process as SP

-- Xterm colors (\033[1;31m has no power here)
printInColor :: String -> Xterm.Color -> IO ()
printInColor string color = do
  Xterm.setSGR [Xterm.SetConsoleIntensity Xterm.BoldIntensity]
  Xterm.setSGR [Xterm.SetColor Xterm.Foreground Xterm.Vivid color]
  putStrLn string
  Xterm.setSGR [Xterm.Reset]

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
    quality = show . floor . (*0.7) . read . take 2 . drop 28 . (!!3) . lines
        
-- Compare cells - used to sort them
compareCells :: Ord a => (a, a, a) -> (a, a, a) -> Ordering
compareCells (quality1, enc1, _) (quality2, enc2, _)
  | enc1 < enc2 = GT
  | enc1 > enc2 = LT
  | quality1 < quality2 = GT
  | quality1 > quality2 = LT
  | otherwise = EQ

-- Prints the cells from FormatCells
-- FIX: If (length quality == 1) it looks ugly
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
                 then " 0" ++ show num
                 else " " ++ show num
  
-- Connects to access point
wpaSupplicant :: (String, String, String) -> IO ()
-- Unknown encrypted access point
wpaSupplicant (_, "0", essid) = do 
  putStrLn $ "Attempting to connect to " ++ essid
  putStr "Password: "
  IO.hFlush IO.stdout
  
  -- Create temp WPA PSK
  passwd <- getLine --FIX: Can fail
  putStr "Generating WPA PSK ... "
  IO.hFlush IO.stdout
  (wpapCode, wpapOut, wpapErr) <-SP.readProcessWithExitCode "wpa_passphrase" [essid, passwd] []
  case wpapCode of
    Exit.ExitFailure _ -> do 
      printInColor ("Error\n" ++ wpapErr) Xterm.Red
      Exit.exitFailure
    _ -> return ()
  
  (ioPath, ioHandle) <- IO.openTempFile "/tmp" ("wlan.hs."++essid) --FIX: Can fail?
  printInColor ("OK: " ++ ioPath) Xterm.Green
  IO.hPutStrLn ioHandle wpapOut
  IO.hClose ioHandle
  putStr "Configuring connection ... "
  IO.hFlush IO.stdout
  
  -- Try connecting
  (wpasCode, _, wpasErr) <- SP.readProcessWithExitCode "wpa_supplicant"
                            (words ("-B -i wlan0 -D nl80211 -c "++ioPath)) []
  case wpasCode of
    Exit.ExitFailure _ -> do
      printInColor ("Error\n" ++ wpasErr) Xterm.Red
      Exit.exitFailure
    _ -> printInColor "OK" Xterm.Green
  dhClient
  
  -- If successfull - save the WPA PSK
  putStr "Storing password for later use ... "
  IO.hFlush IO.stdout
  (mvCode, _, mvErr) <- SP.readProcessWithExitCode "mv" [ioPath, "/root/wlan/"++essid] []
  case mvCode of
    Exit.ExitFailure _ -> do
      printInColor ("Error\n" ++ mvErr) Xterm.Red
      Exit.exitFailure
    _ -> printInColor "OK" Xterm.Green
    
-- Known / unencrypted    
wpaSupplicant (_, enc, essid) = do
  putStrLn $ "Attempting to connect to " ++ essid
  putStr "Configuring connection ... "
  IO.hFlush IO.stdout
  
  (wpaCode, _, wpaErr) <- 
    case enc of
      "2" -> SP.readProcessWithExitCode "wpa_supplicant"
             (words ("-B -i wlan0 -D nl80211 -c /root/wlan/" ++ essid)) []
      _ -> SP.readProcessWithExitCode "iwconfig" ["wlan0", "essid", essid] []
  
  case wpaCode of
    Exit.ExitFailure _ -> do
      printInColor ("Error\n" ++ wpaErr) Xterm.Red
      Exit.exitFailure
    _ -> printInColor "OK" Xterm.Green
  dhClient
        
-- Start dhcp service
dhClient :: IO ()      
dhClient = do
  putStr "Waiting for an IP address ... "
  IO.hFlush IO.stdout
  (dhCode, _, dhErr) <- SP.readProcessWithExitCode "dhclient" ["wlan0", "-1"] []
  case dhCode of 
    Exit.ExitFailure _ -> do -- FIX: Kill wpa_supplicant here
      printInColor ("Error\n" ++ dhErr) Xterm.Red
      Exit.exitFailure
    _ -> printInColor "OK" Xterm.Green

-- Kill processes
wlanKill :: IO ()
wlanKill = do
  let processes = ["dhclient", "wpa_supplicant"] in do
    putStr $ ("Killing " ++ (unwords (L.intersperse "/" processes)) ++ " ... ")
    IO.hFlush IO.stdout
    (killCode, _, killErr) <- SP.readProcessWithExitCode "killall" processes []
  
    case killCode of
      Exit.ExitFailure _ -> printInColor ("Error\n" ++ killErr) Xterm.Red
      _ -> printInColor "OK" Xterm.Green
  
  putStr "Setting wlan0 to up ... "
  IO.hFlush IO.stdout
  Unistd.sleep 2
  (ifCode, _, ifErr) <- SP.readProcessWithExitCode "ifconfig" (words "wlan0 up") []
  case ifCode of
    Exit.ExitFailure _ -> printInColor ("Error\n" ++ ifErr) Xterm.Red
    _ -> printInColor "OK" Xterm.Green
    
  Exit.exitSuccess

connect :: String -> IO ()
connect mode = do
  -- List known access points
  putStr "Checking for known access points ... "
  IO.hFlush IO.stdout
  (dirCode, dirOut, dirErr) <- SP.readProcessWithExitCode "ls" ["/root/wlan"] []
  case dirCode of
    Exit.ExitFailure _ -> do
      printInColor ("Error\n" ++ dirErr) Xterm.Red
      Exit.exitFailure
    _ -> printInColor "OK" Xterm.Green  
      
  -- Try to check for nearby access points
  putStr "Scanning for access points ... "
  IO.hFlush IO.stdout
  (iwCode, iwOut, iwErr) <- SP.readProcessWithExitCode "iwlist" ["wlan0", "scan"] []
  case iwCode of
    Exit.ExitFailure _ -> do
      printInColor ("Error\n" ++ iwErr) Xterm.Red
      Exit.exitFailure
    _ -> printInColor "OK" Xterm.Green
    
  -- Print them out
  let knownCells = L.sortBy compareCells $ flip (formatCells) iwOut $ lines dirOut
  printCells knownCells
  
  -- Select manually or automatically (automatically will only connect to known networks)
  case mode of
    "list" -> Exit.exitSuccess
    "auto" -> do
      let topCell@(q, c, e) = (knownCells !! 0) in
        if c == "2" 
        then wpaSupplicant topCell
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
  
  -- Check if root
  user <- Env.getEnv "USER"
  case user of
    "root" -> return ()
    _ -> do 
      putStrLn "You need to run this application as root"
      Exit.exitFailure

  -- Check for argvs
  argv <- Env.getArgs
  case unwords argv of
    "auto" -> connect "auto"
    "man" -> connect "man"
    "list" -> connect "list"
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
