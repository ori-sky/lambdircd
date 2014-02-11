import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import Control.Concurrent (forkIO)

main :: IO()
main = withSocketsDo $ do
	args <- getArgs
	sock <- listenOn $ PortNumber 6667
	putStrLn "Listening on 6667"
	handleSock sock

handleSock :: Socket -> IO ()
handleSock sock = do
	(handle, _, _) <- accept sock
	hSetBuffering handle NoBuffering
	putStrLn "Accepted connection"
	forkIO $ processSock handle
	handleSock sock

processSock :: Handle -> IO ()
processSock handle = do
	line <- hGetLine handle
	putStrLn line
	processSock handle
