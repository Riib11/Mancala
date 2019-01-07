module Debug
( debug
) where

_DEBUG = False

debug :: Show a => a -> IO ()
debug x = if _DEBUG
    then putStrLn $ "[*] " ++ show x
    else putStr ""
