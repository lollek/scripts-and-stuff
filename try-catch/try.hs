module Main where

import qualified System.Directory as Dir
import qualified Control.Exception as E
import qualified System.IO as IO
import qualified System.IO.Error as IOE

fetchInput :: IO ()
fetchInput = do
  line <- getLine
  print line

errorHandler :: IOError -> IO ()
errorHandler e = putStrLn $ "Error: " ++ IOE.ioeGetErrorString e

main = do
  putStr "Which directory do you want to peek at? "
  IO.hFlush IO.stdout
  
  fetchInput `E.catch` errorHandler
