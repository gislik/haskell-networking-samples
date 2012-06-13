module Main where

import System.Environment (getArgs)
import System.IO (hClose, hPutStrLn, hGetLine, hSetBuffering, BufferMode(..))
import System.IO.Error (catchIOError)
import Control.Exception (finally)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (newChan, writeChan, readChan, dupChan)
import Network

main = withSocketsDo $ do  -- only needed for win32 to initialize the networking subsystem
	channel <- newChan
	args <- getArgs
	let port = read (head args) 
	putStrLn "ChatServer starting..."
	socket <- listenOn $ PortNumber (fromIntegral port)
	acceptLoop socket channel `finally` sClose socket

acceptLoop socket channel = do
	(handle, _, _) <- accept socket
	hSetBuffering handle LineBuffering
	clientChannel <- dupChan channel
	clientId <- forkIO $ clientWriteLoop handle clientChannel
	putStrLn $ "Client " ++ show handle ++ " connected so I spawned " ++ show clientId ++ " to handle writes"
	forkIO $ clientReadLoop handle clientChannel `catchIOError` errorHandler handle clientId `finally` hClose handle
	acceptLoop socket channel

	where errorHandler h t e = do 
		putStrLn ("Client " ++ show h ++ " disconnected so I am killing " ++ show t) 
		killThread t

clientReadLoop handle channel = do
	message <- hGetLine handle
	putStrLn $ "Client " ++ show handle ++ " said: " ++ message
	writeChan channel message
	clientReadLoop handle channel

clientWriteLoop handle channel = do
	message <- readChan channel
	hPutStrLn handle message
	clientWriteLoop handle channel
