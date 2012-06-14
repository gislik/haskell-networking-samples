module Main where

import System.Environment (getArgs)
import System.IO (hClose, hPutStrLn, hGetLine, hSetBuffering, BufferMode(..))
import System.IO.Error (catchIOError, mkIOError, eofErrorType, ioError)
import Control.Exception (finally)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan, dupChan)
import Network
import Data.String (words, unwords)
import Data.Maybe (Maybe (..))
import Text.Printf (printf)

data ServerMessage = ServerMessage {
		messageChannel :: ClientChannel,
		messageCommand :: IrcCommand
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
type MessageString	= String
type ErrorString 	= String

-- TODO: Ping Quit
data IrcCommand = Nick Nickname
				| User Username Hostname Servername RealName
				| Ison Username
				| Userhost Username
				| Quit 
				-- | Join [ChannelName] [Password]
				-- | Part [ChannelName]
				| Privmsg [Receiver] MessageString
				-- | Notice Nickname String
				| Unknown ErrorString

data IrcMessage = IrcMessage {
		ircMessageFrom :: Maybe String,
		ircMessageCommand :: IrcCommand
	}

data IrcReply = RplWelcome 
			  | RplUserhost
			  | RplIson 

instance Enum IrcReply where
	fromEnum RplWelcome = 1
	fromEnum RplUserhost = 302
	fromEnum RplIson = 303

	toEnum 1 = RplWelcome
	toEnum 302 = RplUserhost
	toEnum 303 = RplIson

data IrcReplyMessage = IrcReplyMessage {
		ircReplyMessageReply :: IrcReply,
		ircReplyMessageNick :: Nickname,
		ircReplyMessageString :: MessageString
	}

data IrcOldReply = IrcOldReply {
		ircReplyNumber :: Int,
		ircReplyMessage :: String
	}


type ClientChannel = Chan ClientMessage
data Client = Client Nickname ClientChannel

data ClientMessage = ClientCommand IrcCommand 
				   | ClientReply IrcReplyMessage 
				   | ClientPrivmsg Nickname Nickname MessageString
				   | ClientError ErrorString
				   | ClientQuit 
		
serverName = ":localhost"

main = withSocketsDo $ do  -- only needed for win32 to initialize the networking subsystem
	serverChannel <- newChan
	args <- getArgs
	let port = read (head args) 
	putStrLn "IrcServer starting..."
	socket <- listenOn $ PortNumber (fromIntegral port)
	forkIO $ serverLoop serverChannel [] 
	acceptLoop socket serverChannel `finally` sClose socket

acceptLoop socket serverChannel = do
	(handle, _, _) <- accept socket
	hSetBuffering handle LineBuffering
	clientChannel <- newChan
	clientId <- forkIO $ clientWriteLoop handle clientChannel
	putStrLn $ "Client " ++ show handle ++ " connected so I spawned " 
				++ show clientId ++ " to handle writes"
	forkIO $ clientReadLoop handle clientChannel serverChannel 
				`catchIOError` errorHandler handle clientId
				`finally` hClose handle
	acceptLoop socket serverChannel

	where errorHandler h ct e = do 
		putStrLn $ "Client " ++ show h ++ " disconnected so I am killing " ++ show ct
		killThread ct

clientReadLoop handle clientChannel serverChannel = do
	message <- hGetLine handle
	putStrLn $ "Client " ++ show handle ++ " said: " ++ message
	let ircMessage = parseIrcMessage (words message)
	case (ircMessageCommand ircMessage) of 
		Quit 			-> do
							writeChan serverChannel (ServerMessage clientChannel Quit)
							ioError $ mkIOError eofErrorType "eof" Nothing Nothing
		cmd 			-> do
							writeChan serverChannel (ServerMessage clientChannel cmd)
							clientReadLoop handle clientChannel serverChannel
	
clientWriteLoop handle clientChannel = do
	clientMessage <- readChan clientChannel
	case clientMessage of 
		ClientCommand (Nick nick) -> clientWriteLoop handle clientChannel
		ClientReply (IrcReplyMessage ircReply nick reply) -> do 
			hPutStrLn handle $ serverName ++ " " ++ (printf "%03d" (fromEnum ircReply)) ++ " " ++ nick ++ " " ++  reply
			clientWriteLoop handle clientChannel
		ClientPrivmsg from to msg -> do
			hPutStrLn handle $ (':':from) ++ " " ++ "PRIVMSG " ++ to ++ " " ++ msg
		ClientError err -> do
			hPutStrLn handle $ "ERROR " ++ err

serverLoop serverChannel clients = do
	putStrLn $ "Server holds " ++ show (length clients)
	ServerMessage clientChannel ircCommand <- readChan serverChannel
	case ircCommand of 
		Nick nick 	 		-> do 
				putStrLn $ "Server is adding " ++ nick
				let client = Client nick clientChannel
				respond [client] ircCommand
				serverLoop serverChannel $ client:clients
		Quit   		 		-> serverLoop serverChannel $ filterNotChannel clientChannel
		Privmsg (nick:_) m 	-> do
				let (Client fromNick _) = head (filterChannel clientChannel)
				respond (filterNick nick fromNick) ircCommand
				serverLoop serverChannel clients
		_      		 		-> do
				respond (filterChannel clientChannel) ircCommand
				serverLoop serverChannel clients 

	where 
		filterChannel c = filter (\(Client _ channel) -> channel == c) clients
		filterNotChannel c = filter (\(Client _ channel) -> channel /= c) clients		
		filterNick n fn = map (\(Client _ c) -> Client fn c) $ filter (\(Client nick _) -> nick == n) clients


parse ("NICK":nick:_) = Nick nick
parse ("USER":nick:host:server:real:_) = User nick host server real
parse ("PRIVMSG":r:m) = Privmsg [r] (unwords m)
parse ("ISON":nick:_) = Ison nick
parse ("USERHOST":nick:_) = Userhost nick
parse ("QUIT":_) = Quit 
parse ss = Unknown (unwords ss)

respond _ (User _ _ _ _) = return ()
respond ((Client fromNick c):_) (Nick _) = writeChan c $ ClientReply (IrcReplyMessage  RplWelcome fromNick ":Welcome")
respond ((Client fromNick c):[]) (Privmsg [toNick] msg) = writeChan c $ ClientPrivmsg fromNick toNick msg
respond ((Client fromNick c):cs) cmd@(Privmsg [toNick] msg) = do
									writeChan c $ ClientPrivmsg fromNick toNick msg
									respond cs cmd
respond ((Client fromNick c):_) (Ison nick) = writeChan c $ ClientReply (IrcReplyMessage RplIson fromNick nick)
respond ((Client fromNick c):_) (Userhost nick) = writeChan c $ ClientReply (IrcReplyMessage RplUserhost fromNick (printf ":%s=-~%s@%s" nick nick "127.0.0.1"))
respond ((Client _ c):_) (Unknown err) = writeChan c $ ClientError err
respond [] _  = fail "No matching clients"
respond _ _ = fail "This case is not handled"

parseIrcMessage (s:ss) | head s == ':' = IrcMessage (Just (tail s)) (parse ss)
parseIrcMessage ss = IrcMessage Nothing (parse ss)
