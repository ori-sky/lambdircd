{- Copyright 2014 David Farrell <shokku.ra@gmail.com>

 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at

 - http://www.apache.org/licenses/LICENSE-2.0

 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module IRCD.Server where

import Control.Monad.State
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Network.Socket
import System.IO
import System.IO.Error (tryIOError)
import qualified IRCD.TS6 as TS6
import IRCD.Types.Env (Env, defaultEnv)
import IRCD.Types.Client (uid, defaultClient)
import IRCD.Clients (insertClient)
import IRCD.Env (mapClients)

data Notification = Accept Handle
                  | Recv Int String
                    deriving Show

serveIRC :: IO ()
serveIRC = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock NoDelay 1
    setSocketOption sock ReuseAddr 1
    {- 6 = TCP option, 9 = defer accept
     - only supported on GNU systems
     -}
    tryIOError $ setSocketOption sock (CustomSockOpt (6, 9)) 30
        >> putStrLn "Using deferred accept for connections"
    inet_addr "127.0.0.1" >>= bind sock . SockAddrInet 6667
    listen sock 5
    putStrLn $ "Listening on 127.0.0.1" ++ ":6667"
    chan <- newChan
    forkIO (acceptLoop chan sock)
    evalStateT (mainLoop chan sock) defaultEnv

acceptLoop :: Chan Notification -> Socket -> IO ()
acceptLoop chan sock = do
    tryIOError $ do
        (clientSock, sockAddr) <- accept sock
        handle <- socketToHandle clientSock ReadWriteMode
        hSetNewlineMode handle universalNewlineMode
        hSetBuffering handle LineBuffering
        hSetEncoding handle utf8
        writeChan chan (Accept handle)
    acceptLoop chan sock

inputLoop :: Chan Notification -> Socket -> Handle -> Int -> IO ()
inputLoop chan sock handle uid = do
    hGetLine handle >>= writeChan chan . Recv uid
    inputLoop chan sock handle uid

mainLoop :: Chan Notification -> Socket -> StateT Env IO ()
mainLoop chan sock = do
    note <- liftIO (readChan chan)
    case note of
        Accept handle -> do
            modify $ mapClients (insertClient defaultClient {uid=Just 5000})
            void $ liftIO $ forkIO (inputLoop chan sock handle 5000)
        Recv uid line -> liftIO $ putStrLn ("[::" ++ TS6.intToID uid ++ "] " ++ line)
    mainLoop chan sock
