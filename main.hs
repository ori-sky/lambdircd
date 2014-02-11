import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad

data User = User
    { handle :: Maybe Handle
    , nick   :: Maybe String
    , user   :: Maybe String
    , real   :: Maybe String
    }

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    usersT <- atomically $ do newTVar []
    sock <- listenOn $ PortNumber 6667
    forkIO $ acceptLoop usersT sock
    mainLoop usersT

acceptLoop :: TVar [User] -> Socket -> IO ()
acceptLoop usersT sock = do
    (handle, _, _) <- accept sock
    atomically $ do
        users <- readTVar usersT
        writeTVar usersT $ User (Just handle) Nothing Nothing Nothing : users
    acceptLoop usersT sock

mainLoop :: TVar [User] -> IO ()
mainLoop usersT = do
    users <- atomically $ do readTVar usersT
    putStrLn $ show $ length users
    threadDelay 100000
    mainLoop usersT
