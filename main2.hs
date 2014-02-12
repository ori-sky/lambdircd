import System.Environment (getArgs)
import System.IO
import Network
import Network.Socket
import Network.SocketServer
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad

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

main :: IO ()
main = do
    serveTCPforever (simpleTCPOptions 6667) {reuse = True}
        $ threadedHandler $ handleHandler
        (\h _ _ -> do
            putStrLn "hello"
            hPutStrLn h "hello"
        )
{-
main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    sharedT <- atomically $ do newTVar $ Shared []
    sock <- listenOn $ PortNumber 6667
    forkIO $ acceptLoop sharedT sock
    mainLoop sharedT $ Env []

acceptLoop :: TVar Shared -> Socket -> IO ()
acceptLoop sharedT sock = do
    (s, _) <- accept sock
    atomically $ do
        shared <- readTVar sharedT
        writeTVar sharedT $ Shared (s : sockets shared)
    acceptLoop sharedT sock

mainLoop :: TVar Shared -> Env -> IO ()
mainLoop sharedT env = do
    shared <- atomically $ do readTVar sharedT
    putStrLn $ show (sockets shared)
    --hs <- filterM hIsWritable (handles shared)
    --putStrLn $ show hs
    threadDelay 1000000
    mainLoop sharedT env
-}
