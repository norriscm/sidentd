module SimpleIdentHandler (handleQuery) where

user :: String -- the static user
user = "cain"

handleQuery :: Int -> Int -> IO (String, String) -- do the lookup
handleQuery lport fport = return ("USERID","UNIX : " ++ user)



