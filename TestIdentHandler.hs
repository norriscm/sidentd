module TestIdentHandler (handleQuery) where

handleQuery :: Int -> Int -> IO (String, String) -- do the lookup
handleQuery lport fport = return ("USERID","UNIX : cain")



