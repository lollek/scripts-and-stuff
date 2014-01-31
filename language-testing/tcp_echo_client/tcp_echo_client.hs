module Main where

import System.IO

import Data.Char (isDigit)
import Network (connectTo, PortID(PortNumber))
import System.Environment (getProgName, getArgs)
import Text.Printf (printf)

raise :: String -> IO ()
raise err = do
  getProgName >>= hPutStrLn stderr . printf "Usage: %s hostname port"
  hPutStrLn stderr $ printf "Error: %s" err

sendReceiveData :: String -> String -> IO ()
sendReceiveData hostname port = do
  hSock <- connectTo hostname $ PortNumber $ toEnum $ read port
  hSetBuffering stdin LineBuffering
  hSetBuffering hSock LineBuffering
  hGetContents stdin >>= hPutStrLn hSock
  hGetContents hSock >>=
    (\content -> if last content == '\n'
                  then hPutStr   stdout content
                  else hPutStrLn stdout content)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> raise "No hostname given!"
    1 -> raise "No port given!"
    2 -> let [hostname, port] = args in
          if (all isDigit) port
            then sendReceiveData hostname port
            else raise "Port contains non-digits!"
    _ -> raise "Too many arguments given!"
