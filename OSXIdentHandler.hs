module OSXIdentHandler (handleQuery) where

import System.Process (runInteractiveCommand, terminateProcess)
import System.IO

handleQuery :: Int -> Int -> IO (String, String) -- do the lookup
handleQuery lport fport = do
  putStrLn (syse lport fport)
  (_,out,_,proc) <- runInteractiveCommand (syse lport fport)
  hSetBuffering out LineBuffering
  name <- hGetLine out
  hClose out
  if (length name > 0)
    then return ("USERID", "UNIX : " ++ name)
    else return ("ERROR","NO USER")

syse :: Int -> Int -> String
syse lport fport =
  "lsof -nP -iTCP | " ++  --list all open TCP connections numerically
  "awk '$9~/^.+:" ++ (show lport) ++ ".+:" ++ (show fport) ++ "$/ " ++  --if the address matches
  "{ print $3; }; " ++                                              --print the username
  "END { print \"\n\";}'" --always print a newline, so hGetLine never fails
