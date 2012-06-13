module Main where

import System.Environment (getArgs)
import System.IO (hClose, hPutStrLn)
import Control.Exception (finally)
import Network

main = withSocketsDo $ do  -- only needed for win32 to initialize the networking subsystem
	args <- getArgs
	let port = read (head args) 
	putStrLn "HelloServer starting..."
	socket <- listenOn $ PortNumber (fromIntegral port)
	acceptLoop socket `finally` sClose socket

acceptLoop socket = do
	(handle, _, _) <- accept socket
	hPutStrLn handle "Hello World" `finally` hClose handle
	acceptLoop socket
