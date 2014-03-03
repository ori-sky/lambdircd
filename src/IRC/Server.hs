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

module IRC.Server
( serveIRC
) where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import System.IO
import System.IO.Error
import System.Timeout
import Network hiding (accept)
import Network.Socket
import IRC.Message
import IRC.Numeric
import IRC.Server.Client (defaultClient, isClientRegistered, sendClient)
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Options as Opts
import qualified IRC.Server.Environment as Env
import qualified Plugin as P
import Plugin.Load

serveIRC :: Env.Env -> IO ()
serveIRC env = withSocketsDo $ do
    plugins <- mapM loadPlugin pluginNames
    sharedT <- atomically $ newTVar Env.defaultShared
    let handlers = M.unions $ map (M.fromList.P.handlers) (catMaybes plugins)
    let newEnv = env {Env.handlers=handlers, Env.shared=Just sharedT}

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    {- 6 = TCP option, 9 = defer accept
     - only supported on GNU systems
     -}
    tryIOError $ setSocketOption sock (CustomSockOpt (6, 9)) 30
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    listen sock 5

    acceptLoop sock newEnv
  where Env.Env {Env.options=Opts.Options {Opts.plugins=pluginNames, Opts.port=port}} = env

acceptLoop :: Socket -> Env.Env-> IO ()
acceptLoop sock env = do
    tryIOError $ do
        (clientSock, sockAddr) <- accept sock
        handle <- socketToHandle clientSock ReadWriteMode
        hSetNewlineMode handle universalNewlineMode
        hSetBuffering handle LineBuffering
        hSetEncoding handle utf8
        let client = defaultClient {Client.handle=Just handle}
        let newEnv = env {Env.client=client}

        forkIO (serveClient newEnv sockAddr)
    acceptLoop sock env

lookupHost :: SockAddr -> IO String
lookupHost sockAddr = catchIOError f c
  where
    f = do
        (Just hostName, Nothing) <- getNameInfo [] True False sockAddr
        return hostName
    c = (\_ -> do
            (Just ip, Nothing) <- getNameInfo [NI_NUMERICHOST] True False sockAddr
            return ip
        )

serveClient :: Env.Env -> SockAddr -> IO ()
serveClient env sockAddr = do
    host <- lookupHost sockAddr

    shared <- atomically $ readTVar sharedT
    let uid = if IM.null (Env.clients shared) then 1
        else fst (IM.findMax (Env.clients shared)) + 1

    tryIOError $ do
        maybeEnv <- timeout (toMicro connectTimeout) $ registerClient env
            { Env.client = client
                { Client.uid    = Just uid
                , Client.host   = Just host
                }
            }
        case maybeEnv of
            Just newEnv -> do
                sendNumeric newEnv (Numeric 1) ["Welcome to lambdircd " ++ nick]
                sendNumeric newEnv (Numeric 2) ["Your host is just.nothing[0.0.0.0/6667], running lambdircd"]
                sendNumeric newEnv (Numeric 3) ["This server was created (Just now)"]
                sendNumeric newEnv (Numeric 375) ["- lambdircd Message of the Day -"]
                sendNumeric newEnv (Numeric 372) ["- Welcome to lambdircd"]
                sendNumeric newEnv (Numeric 376) ["End of /MOTD command"]
                sendNumeric newEnv (Numeric 0) ["Your host is `" ++ fromJust (Client.host newClient) ++ "`"]
                loopClient newEnv False
              where
                newClient = Env.client newEnv
                Just nick = Client.nick newClient
            Nothing -> return ()
    atomically $ do
        shared <- readTVar sharedT
        let newClients = IM.delete uid (Env.clients shared)
        case IM.lookup uid (Env.clients shared) of
            Just (Client.Client {Client.nick=Just nick}) -> do
                let newUids = M.delete nick (Env.uids shared)
                writeTVar sharedT shared {Env.clients=newClients, Env.uids=newUids}
            _ -> writeTVar sharedT shared {Env.clients=newClients}
    tryIOError $ hClose handle
    return ()
  where
    Just sharedT = Env.shared env
    client = Env.client env
    Just handle = Client.handle client
    Env.Env {Env.options=Opts.Options {Opts.connectTimeout=connectTimeout}} = env

registerClient :: Env.Env -> IO Env.Env
registerClient env = do
    line <- hGetLine handle
    let msg = parseMessage line
    case M.lookup (command msg) (Env.handlers env) of
        Just handler -> do
            newEnv <- handler env msg
            case isClientRegistered (Env.client newEnv) of
                True    -> return newEnv
                False   -> registerClient newEnv
        Nothing -> registerClient env
  where Just handle = Client.handle (Env.client env)

loopClient :: Env.Env -> Bool -> IO ()
loopClient env pinged = do
    maybeEnv <- timeout (toMicro pingTimeout) (handleLine env)
    case maybeEnv of
        Just newEnv -> loopClient newEnv False
        Nothing     -> case pinged of
            True        -> return ()
            False       -> sendClient client "PING :lambdircd"
                            >> loopClient env True
  where
    pingTimeout = Opts.pingTimeout (Env.options env)
    client = Env.client env

handleLine :: Env.Env -> IO Env.Env
handleLine env = do
    uids <- atomically $ do
        shared <- readTVar sharedT
        let newClients = IM.insert uid client (Env.clients shared)
        let newUids = case IM.lookup uid (Env.clients shared) of
                Just Client.Client {Client.nick=Just oldNick} -> do
                    M.insert nick uid $ M.delete oldNick (Env.uids shared)
                _ -> do
                    M.insert nick uid (Env.uids shared)
        writeTVar sharedT shared {Env.clients=newClients, Env.uids=newUids}
        return newUids
    print uids

    line <- hGetLine handle
    let msg = parseMessage line
    case M.lookup (command msg) (Env.handlers env) of
        Just handler -> do
            let newEnv = handler env msg
            newEnv
        Nothing -> do
            sendNumeric env (Numeric 431) [command msg, "Unknown command"]
            handleLine env
  where
    Just sharedT = Env.shared env
    client = Env.client env
    Just handle = Client.handle client
    Just uid = Client.uid client
    Just nick = Client.nick client
    -- force thread crash if Nothing which should never happen here

toMicro :: Num a => a -> a
toMicro = (*1000000)
