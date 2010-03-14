module UNIXIdentHandler (unixHandler) where

import System.Process (runInteractiveCommand)
import System.IO

unixHandler :: (Int -> Int -> String) -> Int -> Int -> IO (String, String) -- do the lookup
unixHandler syse lport fport = do
  putStrLn (syse lport fport)
  (_,out,_,proc) <- runInteractiveCommand (syse lport fport)
  hSetBuffering out LineBuffering
  name <- hGetLine out
  hClose out
  if (length name > 0)
    then return ("USERID", "UNIX : " ++ name)
    else return ("ERROR","NO USER")
