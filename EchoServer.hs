module Main where

import System.Environment (getArgs)
import System.IO (hClose, hPutStrLn, hGetLine, hSetBuffering, BufferMode(..))
import System.IO.Error (catchIOError)
import Control.Exception (finally)
import Control.Concurrent (forkIO)
import Network

main = withSocketsDo $ do  -- only needed for win32 to initialize the networking subsystem
	args <- getArgs
	let port = read (head args) 
	putStrLn "EchoServer starting..."
	socket <- listenOn $ PortNumber (fromIntegral port)
	acceptLoop socket `finally` sClose socket

acceptLoop socket = do
	(handle, _, _) <- accept socket
	hSetBuffering handle LineBuffering
	forkIO $ clientLoop handle `catchIOError` errorHandler handle `finally` hClose handle
	acceptLoop socket

	where errorHandler h e = putStrLn $ "Client " ++ show h ++ " disconnected"

clientLoop handle = do
	message <- hGetLine handle
	putStrLn $ "Client " ++ show handle ++ " said: " ++ message
	hPutStrLn handle message
	clientLoop handle
