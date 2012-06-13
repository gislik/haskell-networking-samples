module Main where

import System.Environment (getArgs)
import System.IO (hClose, hPutStrLn, hGetLine, hSetBuffering, BufferMode(..))
import System.IO.Error (catchIOError)
import Control.Exception (finally)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan, dupChan)
import Network
import Data.String (words, unwords)
import Data.Maybe (Maybe (..))

data Message = Message {
		channel :: Chan Message,
		message :: String
	}

type Username 		= String
type Password 		= String
type Nickname 		= String
type Hostname 		= String
type Servername 	= String
type RealName 		= String
type QuitMessage 	= String
type ChannelName 	= String
type Receiver 		= String

-- TODO: Ping Quit
data IRCCommand = Pass Password
				| Nick Nickname
				| User Username Hostname Servername RealName
				| Ison Username
				| Quit QuitMessage
				| Join [ChannelName] [Password]
				| Part [ChannelName]
				| Privmsg [Receiver] String
				| Notice Nickname String
				| Unknown String

instance Show IRCCommand where
	show (Nick nick) = "NICK " ++ nick
	show (User nick host server real) = "USER " ++ nick ++ " " ++ host ++ " " ++ server ++ " " ++ real
	show (Ison user) = "ISON " ++ user
	show (Privmsg [r] m) = "PRIVMSG " ++ r ++ " " ++ m
	show (Unknown cmd) = "ERROR :Unknown command '" ++ cmd ++ "'"
	show _ = "ERROR :server error"

data IRCMessage = IRCMessage {
		from :: Maybe String,
		command :: IRCCommand
	}

instance Show IRCMessage where
	show (IRCMessage (Just from) command) = (':':from) ++ show command
	show (IRCMessage Nothing command) = show command


main = withSocketsDo $ do  -- only needed for win32 to initialize the networking subsystem
	broadcastChannel <- newChan
	serverChannel <- newChan
	args <- getArgs
	let port = read (head args) 
	putStrLn "IrcServer starting..."
	socket <- listenOn $ PortNumber (fromIntegral port)
	acceptLoop socket serverChannel broadcastChannel `finally` sClose socket

acceptLoop socket serverChannel channel  = do
	(handle, _, _) <- accept socket
	hSetBuffering handle LineBuffering
	broadcastChannel <- dupChan channel
	clientChannel <- newChan
	clientId <- forkIO $ clientWriteLoop handle clientChannel
	broadcastId <- forkIO $ clientBroadcastLoop handle broadcastChannel
	putStrLn $ "Client " ++ show handle ++ " connected so I spawned " 
				++ show clientId ++ " to handle writes and "
				++ show broadcastId ++ " to handle broadcasts"
	forkIO $ clientReadLoop handle clientChannel serverChannel 
				`catchIOError` errorHandler handle clientId broadcastId
				`finally` hClose handle
	forkIO $ serverLoop serverChannel broadcastChannel
	acceptLoop socket serverChannel channel

	where errorHandler h ct bt e = do 
		putStrLn $ "Client " ++ show h ++ " disconnected so I am killing " ++ show ct
						++ " and " ++ show bt 
		killThread ct
		killThread bt

clientReadLoop handle clientChannel serverChannel = do
	message <- hGetLine handle
	putStrLn $ "Client " ++ show handle ++ " said: " ++ message
	writeChan serverChannel (Message clientChannel message)
	clientReadLoop handle clientChannel serverChannel 

clientWriteLoop handle clientChannel = do
	Message channel message <- readChan clientChannel
	hPutStrLn handle message
	clientWriteLoop handle clientChannel

clientBroadcastLoop handle broadcastChannel = do
	Message channel message <- readChan broadcastChannel
	hPutStrLn handle message
	clientBroadcastLoop handle broadcastChannel

serverLoop serverChannel broadcastChannel = do
	Message channel message' <- readChan serverChannel
	let ircMessage = parseIRCMessage (words message')
	-- respond serverChannel broadcastChannel channel (command message'') 
	respond' channel ircMessage
	serverLoop serverChannel broadcastChannel

	where respond' channel imsg = respond serverChannel broadcastChannel channel (command imsg)

respond :: Chan Message -> Chan Message -> Chan Message -> IRCCommand -> IO ()
respond s b c (Nick _) = return ()
respond s b c (User _ _ _ _) = writeChan b (Message s "001 gislik :Welcome")
respond s b c cmd@(Privmsg _ _) = writeChan b (Message s (show cmd))
respond s b c (Ison nick) = writeChan c (Message s ("303 unused :" ++ nick))
respond s b c cmd@(Unknown _) = writeChan s (Message s (show cmd))
respond _ _ _ _ = fail "This case is not handled"

parseIRCMessage (s:ss) | head s == ':' = IRCMessage (Just (tail s)) (parse ss)
parseIRCMessage ss = IRCMessage Nothing (parse ss)


parse ("NICK":nick:_) = Nick nick
parse ("USER":nick:host:server:real:_) = User nick host server real
parse ("PASS":password:ss) = Pass password
parse ("PRIVMSG":r:m:_) = Privmsg [r] m
parse ("ISON":nick:_) = Ison nick
parse ss = Unknown (unwords ss)
