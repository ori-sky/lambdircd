import System.Environment (getArgs)
--import System.IO (hSetBuffering, hGetLine, hPutStrLn, hClose, hIsEOF, hIsOpen, BufferMode(..), Handle)
import System.IO
import Network (withSocketsDo, listenOn, accept, Socket, PortID(..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
--import Control.Monad (liftM, filterM)
import Control.Monad

data Shared = Shared
    { handles :: [Handle]
    }

data User = User
    { handle :: Maybe Handle
    , id     :: Maybe String
    , nick   :: Maybe String
    , user   :: Maybe String
    , real   :: Maybe String
    }

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    sharedT <- atomically $ do newTVar $ Shared []
    sock <- listenOn $ PortNumber 6667
    forkIO $ acceptLoop sharedT sock
    mainLoop sharedT

acceptLoop :: TVar Shared -> Socket -> IO ()
acceptLoop sharedT sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    atomically $ do
        shared <- readTVar sharedT
        writeTVar sharedT $ Shared (handle : handles shared)
    acceptLoop sharedT sock

mainLoop :: TVar Shared -> IO ()
mainLoop sharedT = do
    shared <- atomically $ do readTVar sharedT
    hs <- filterM hIsWritable (handles shared)
    putStrLn $ show hs
    threadDelay 1000000
    mainLoop sharedT
