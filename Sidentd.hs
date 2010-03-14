module Main (main) where

import Network
import System.IO
import System.Timeout
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import System.Posix.Daemonize (daemonize)
import Char

user = "cain"

main :: IO ()
main = withSocketsDo $ do
  s <- listenOn $ PortNumber 113
  daemonize $ forever $ do
    (h, host, port) <- accept s
    forkIO (handleConnection h)

handleConnection :: Handle -> IO ()  -- this is the control flow of each connection thread
handleConnection h = finally hndlr cleanup where
  hndlr = withTimeout $ do
    hSetBuffering h LineBuffering
    query <- hGetLine h
    hPutStr h (query ++ " : USERID : UNIX : " ++ user)
  cleanup = hClose h
  withTimeout a = do
    r <- timeout (30*(10^6)) a
    return $ case r of {Nothing -> ();Just v  -> v}
