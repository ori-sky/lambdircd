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
            hPutStrLn handle "What's your name?"
            hPutStr handle "> "
            hFlush handle
            line <- hGetLine handle
            let response = concat ["Hello ", line, "!"]
            putStrLn response
            hPutStrLn handle response
        )
