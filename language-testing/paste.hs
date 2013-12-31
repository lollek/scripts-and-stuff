module Main where

import System.IO
import System.Console.GetOpt

import Data.Maybe (fromMaybe)
import System.Environment (getProgName, getArgs)
import System.IO.Error (tryIOError)
import Text.Printf (printf)

-- Part 1: Argument parsing

data Flag = Delim String
  deriving Show
opts :: [OptDescr Flag]
opts = [ Option ['d'] ["delim"] (OptArg delimOpt "DELIM") "delimiter" ]

delimOpt :: Maybe String -> Flag
delimOpt opt = Delim $ fromMaybe "\t" opt

header :: String -> String
header = printf "Usage: %s FILE1 FILE2 [-d[DELIM]+]"

main :: IO ()
main = do
  args <- getArgs
  arg0 <- getProgName
  case getOpt Permute opts args of
    (flags, (file1:file2:[]), []) -> paste file1 file2 flags
    (_, _, msgs) -> error $ concat msgs ++ usageInfo (header arg0) opts

-- Part 2: Paste files

myGetLine :: Handle -> IO (Bool, String)
myGetLine handle = do
  input <- tryIOError $ hGetLine handle
  case input of
    Left _ -> return (False, "")
    Right s -> return (True, s)

printOutput :: (Bool, Handle) -> (Bool, Handle) -> String -> IO ()
printOutput (False, _) (False, _) _ = return ()
printOutput (_, h1) (_, h2) d = do
  (b1, line1) <- myGetLine h1
  (b2, line2) <- myGetLine h2
  putStrLn $ line1 ++ d ++ line2
  printOutput (b1, h1) (b2, h2) d

paste :: String -> String -> [Flag] -> IO ()
paste file1 file2 [Delim d] =
  withFile file1 ReadMode $ \hFile1 ->
    withFile file2 ReadMode $ \hFile2 ->
      printOutput (True, hFile1) (True, hFile2) d
paste file1 file2 [] = paste file1 file2 [Delim "\t"]
paste _ _ _ = error "Bad delimiter - this should not happen"
