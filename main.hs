import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad

data User = User
    { handle :: Maybe Handle
    , nick   :: Maybe String
    , user   :: Maybe String
    , real   :: Maybe String
    }

usersT :: STM (TVar [User])
usersT = newTVar []

type Msg = String

main :: IO()
main = withSocketsDo $ do
    args <- getArgs
    sock <- listenOn $ PortNumber 6667
    forkIO mainLoop
    acceptLoop sock

mainLoop :: IO ()
mainLoop = atomically $ do
    users <- readTVar usersT
    putStrLn $ show $ length users
    mainLoop

acceptLoop :: Socket -> IO ()
acceptLoop sock = do
    (handle, _, _) <- accept sock
    forkIO $ atomically $ do
        users <- readTVar usersT
        writeTVar usersT $ (User handle Nothing Nothing Nothing) : users
    acceptLoop sock
