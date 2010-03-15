module UNIXIdentHandler (unixHandler, handlerInit, HandlerState) where

import System.Process (runInteractiveCommand)
import System.IO

type HandlerState = ()

unixHandler :: (Int -> Int -> String) -> a -> Int -> Int -> IO (String, String) -- do the lookup
unixHandler syse _ lport fport = do
  putStrLn (syse lport fport)
  (_,out,_,proc) <- runInteractiveCommand (syse lport fport)
  hSetBuffering out LineBuffering
  name <- hGetLine out
  hClose out
  if (length name > 0)
    then return ("USERID", "UNIX : " ++ name)
    else return ("ERROR","NO USER")

handlerInit cont = cont undefined
