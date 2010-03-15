module Main (main) where

import Network
import System.IO
import System.Timeout
import System (getArgs, exitFailure)
import Control.Monad (forever, liftM, replicateM)
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import System.Posix (setGroupID, setUserID)
import System.Posix.Daemonize (daemonize)
import Data.Char (isPrint)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: sidentd USER" >> exitFailure
    (user:_) -> do
      s <- listenOn $ PortNumber 113
      setGroupID 1
      setUserID 1
      daemonize $ forever $ do
        (h, host, port) <- accept s
        forkIO (handleConnection user h)

handleConnection :: String -> Handle -> IO ()  -- this is the control flow of each connection thread
handleConnection user h = finally hndlr cleanup where
  hndlr = withTimeout $ do
    hSetBuffering h LineBuffering
    query <- hGetLineN h 512
    hPutStrLn h ((filter isPrint query) ++ " : USERID : UNIX : " ++ user)
  cleanup = hClose h
  withTimeout a = do
    r <- timeout (30*(10^6)) a
    return $ case r of {Nothing -> ();Just v  -> v}

hGetLineN :: Handle -> Int -> IO String
hGetLineN h n = liftM (takeWhile (/= '\n')) $ replicateM n (hGetChar h)
