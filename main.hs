import System.Environment (getArgs)
import System.IO
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

main :: IO ()
main = do
    serveTCPforever (simpleTCPOptions 6667) {reuse = True}
        $ threadedHandler $ handleHandler
        (\handle _ _ -> do
            hSetBuffering handle NoBuffering
            hSetNewlineMode handle universalNewlineMode
            hSetEncoding handle utf8

            putStrLn "connection"
            hPutStr handle ":lambdircd 001 Dashie :Welcome to the lambdircd Internet Relay Network Dashie\r\n"
            hPutStr handle ":Dashie JOIN #test\r\n"
            hPutStr handle ":Pinkie JOIN #test\r\n"
            hPutStr handle ":Shockky JOIN #test\r\n"
            loop
        )

loop :: IO ()
loop = loop
