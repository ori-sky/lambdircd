import System.Environment (getArgs)
import System.IO
import System.Timeout
import Network
import Network.Socket
import Network.SocketServer
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad

{-
data Shared = Shared
    { sockets :: [Socket]
    }

data Client = Client (Maybe Socket)
data Env = Env [Client]
data User = User
    { socket :: Maybe Socket
    , id     :: Maybe String
    , nick   :: Maybe String
    , user   :: Maybe String
    , real   :: Maybe String
    }
-}

data Prefix = StringPrefix String
            | MaskPrefix
                { nick  :: String
                , user  :: String
                , host  :: String
                }

data Message = Message
    { messagePrefix     :: (Maybe Prefix)
    , messageCommand    :: String
    , messageParams     :: [String]
    }

data Client = Client
    { clientHandle      :: Handle
    , clientNick        :: Maybe String
    }

main :: IO ()
main = do
    serveTCPforever (simpleTCPOptions 6667) {reuse = True}
        $ threadedHandler $ handleHandler
        (\handle _ _ -> do
            hSetBuffering handle NoBuffering
            hSetNewlineMode handle universalNewlineMode
            hSetEncoding handle utf8

            let client = Client handle Nothing

            putStrLn "connected"
            timeout 15000000 $ clientHandler client
            putStrLn "disconnected"
            return ()
        )

clientHandler :: Client -> IO ()
clientHandler client = do
    line <- hGetLine (clientHandle client)
    let w:ws = words line
    messageHandler client $ Message Nothing w ws
    clientHandler client

messageHandler :: Client -> Message -> IO ()
messageHandler client (Message _ "NICK" (p:ps)) = putStrLn $ "nick = " ++ p
messageHandler client (Message _ "USER" ps)     = putStrLn $ "user = " ++ (show ps)
messageHandler client (Message _ op ps)         = putStrLn $ op ++ " and " ++ (show ps)
