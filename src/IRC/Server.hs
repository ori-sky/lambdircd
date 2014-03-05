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

import Data.Char (toUpper)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.IO
import System.IO.Error
import System.Timeout
import Network hiding (accept)
import Network.Socket
import IRC.Message
import IRC.Numeric
import IRC.Server.Client (defaultClient, isClientReady, sendClient)
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Options as Opts
import qualified IRC.Server.Environment as Env
import qualified Plugin as P
import Plugin.Load

serveIRC :: Env.Env -> IO ()
serveIRC env = withSocketsDo $ do
    plugins <- mapM loadPlugin pluginNames
    sharedM <- newMVar Env.defaultShared
    let handlers = M.unions $ map (M.fromList.P.handlers) (catMaybes plugins)
    let newEnv = env {Env.handlers=handlers, Env.shared=Just sharedM}

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
    shared <- takeMVar sharedM
    let uid = if IM.null (Env.clients shared)
        then 1
        else fst (IM.findMax (Env.clients shared)) + 1
    tryIOError $ do
        host <- lookupHost sockAddr
        maybeEnv <- timeout (toMicro connectTimeout) $ registerClient env
            { Env.client = client {Client.uid=Just uid, Client.host=Just host}
            , Env.local = shared
            }
        case maybeEnv of
            Just newEnv -> do
                sendNumeric newEnv numRPL_WELCOME   ["Welcome to lambdircd " ++ nick]
                sendNumeric newEnv numRPL_YOURHOST  ["Your host is lambdircd, running lambdircd"]
                sendNumeric newEnv numRPL_CREATED   ["This server was created (Just now)"]
                sendNumeric newEnv numRPL_MOTDSTART ["- lambdircd Message of the Day -"]
                sendNumeric newEnv numRPL_MOTD      ["- Welcome to lambdircd"]
                sendNumeric newEnv numRPL_ENDOFMOTD ["End of /MOTD command"]
                sendNumeric newEnv (Numeric 0) ["Your host is `" ++ fromJust (Client.host newClient) ++ "`"]
                loopClient newEnv {Env.client=newClient {Client.registered=True}} False
              where
                newClient = Env.client newEnv
                Just nick = Client.nick newClient
            Nothing -> return ()
    modifyMVar_ sharedM $
        \shared -> do
            let newClients = IM.delete uid (Env.clients shared)
            return $ case IM.lookup uid (Env.clients shared) of
                Just (Client.Client {Client.nick=Just nick}) -> do
                    let newUids = M.delete (map toUpper nick) (Env.uids shared)
                    shared {Env.clients=newClients, Env.uids=newUids}
                _ -> shared {Env.clients=newClients}
    tryIOError $ hClose handle
    return ()
  where
    Just sharedM = Env.shared env
    client = Env.client env
    Just handle = Client.handle client
    Env.Env {Env.options=Opts.Options {Opts.connectTimeout=connectTimeout}} = env

registerClient :: Env.Env -> IO Env.Env
registerClient env = do
    let newClients = IM.insert uid client (Env.clients local)
        newUids = case maybeNick of
            Just nick -> case IM.lookup uid (Env.clients local) of
                Just Client.Client {Client.nick=Just oldNick}
                    -> M.insert (map toUpper nick) uid $ M.delete (map toUpper oldNick) (Env.uids local)
                _   -> M.insert (map toUpper nick) uid (Env.uids local)
            Nothing -> Env.uids local
    putMVar sharedM local {Env.clients=newClients, Env.uids=newUids}
    line <- hGetLine handle
    shared <- takeMVar sharedM
    let localEnv = env {Env.local=shared}

    let msg = parseMessage line
    case M.lookup (command msg) (Env.handlers localEnv) of
        Just handler -> do
            newEnv <- handler localEnv msg
            case isClientReady (Env.client newEnv) of
                True    -> return newEnv
                False   -> registerClient newEnv
        Nothing -> registerClient localEnv
  where
    Just sharedM = Env.shared env
    local = Env.local env
    client = Env.client env
    Just handle = Client.handle (Env.client env)
    Just uid = Client.uid client
    maybeNick = Client.nick client

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
    let newClients = IM.insert uid client (Env.clients local)
        newUids = case IM.lookup uid (Env.clients local) of
            Just Client.Client {Client.nick=Just oldNick}
                -> M.insert (map toUpper nick) uid $ M.delete (map toUpper oldNick) (Env.uids local)
            _   -> M.insert (map toUpper nick) uid (Env.uids local)
    putMVar sharedM local {Env.clients=newClients, Env.uids=newUids}
    line <- hGetLine handle
    shared <- takeMVar sharedM
    let localEnv = env {Env.local=shared}

    let msg = parseMessage line
    case M.lookup (command msg) (Env.handlers localEnv) of
        Just handler -> handler localEnv msg
        Nothing -> do
            sendNumeric localEnv numERR_UNKNOWNCOMMAND [command msg, "Unknown command"]
            handleLine localEnv
  where
    Just sharedM = Env.shared env
    local = Env.local env
    client = Env.client env
    Just handle = Client.handle client
    Just uid = Client.uid client
    Just nick = Client.nick client
    -- force thread crash if Nothing which should never happen here

toMicro :: Num a => a -> a
toMicro = (*1000000)
