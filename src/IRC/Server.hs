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
import Data.List (sort)
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
import qualified IRC.Server.Environment as Env
import qualified Plugin as P
import Config
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
    bindSocket sock $ SockAddrInet (fromIntegral port) iNADDR_ANY
    listen sock 5

    acceptLoop sock newEnv
  where
    cp = Env.config env
    pluginNames = words $ getConfigString cp "DEFAULT" "plugins"
    port = getConfigInt cp "DEFAULT" "port"

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
        (Just hostName, _) <- getNameInfo [] True False sockAddr
        return hostName
    c = \_ -> do
            (Just ip, _) <- getNameInfo [NI_NUMERICHOST] True False sockAddr
            return ip

serveClient :: Env.Env -> SockAddr -> IO ()
serveClient env sockAddr = do
    tryIOError $ do
        shared <- readMVar sharedM
        host <- lookupHost sockAddr
        let client = baseClient {Client.host=Just host}
        maybeEnv <- timeout (toMicro connectTimeout) $ registerClient env {Env.client=client, Env.local=shared}
        case maybeEnv of
            Just newEnv -> do
                shared <- takeMVar sharedM
                if M.notMember (map toUpper nick) uids
                    then do
                        putMVar sharedM shared {Env.clients=newClients, Env.uids=newUids}
                        sendNumeric regEnv numRPL_WELCOME   ["Welcome to lambdircd " ++ nick]
                        sendNumeric regEnv numRPL_YOURHOST  ["Your host is lambdircd, running lambdircd"]
                        sendNumeric regEnv numRPL_CREATED   ["This server was created (Just now)"]
                        sendNumeric regEnv numRPL_MOTDSTART ["- lambdircd Message of the Day -"]
                        sendNumeric regEnv numRPL_MOTD      ["- Welcome to lambdircd"]
                        sendNumeric regEnv numRPL_ENDOFMOTD ["End of /MOTD command"]
                        sendNumeric regEnv (Numeric 0) ["Your host is `" ++ host ++ "`"]
                        tryIOError $ loopClient regEnv False
                        modifyMVar_ sharedM cleanup
                    else do
                        putMVar sharedM shared
                        sendNumeric regEnv numERR_NICKCOLLISION [nick, "Nickname collision KILL"]
                        sendClient regClient $ "ERROR :Closing Link " ++ host ++ " (Nickname collision KILL)"
              where
                clients     = Env.clients shared
                uids        = Env.uids shared
                uid         = firstAvailable $ sort (IM.keys clients)
                regClient   = (Env.client newEnv) {Client.uid=Just uid, Client.registered=True}
                regEnv      = newEnv {Env.client=regClient}
                newClients  = IM.insert uid regClient clients
                newUids     = M.insert (map toUpper nick) uid uids
                Just nick   = Client.nick regClient
                cleanup s   = do
                    let nc = IM.delete uid (Env.clients s)
                    return $ case IM.lookup uid (Env.clients s) of
                        Just (Client.Client {Client.nick=Just n}) -> s {Env.clients=nc, Env.uids=nu}
                          where nu = M.delete (map toUpper n) (Env.uids s)
                        _ -> s {Env.clients=nc}
            Nothing -> sendClient client $ "ERROR :Closing Link " ++ host ++ " (Connection timed out)"
    tryIOError $ hClose handle
    return ()
  where
    Just sharedM = Env.shared env
    connectTimeout = getConfigInt (Env.config env) "DEFAULT" "connect_timeout"
    baseClient = Env.client env
    Just handle = Client.handle baseClient

registerClient :: Env.Env -> IO Env.Env
registerClient env = do
    line <- hGetLine handle
    let msg = parseMessage line
    case M.lookup (command msg) (Env.handlers env) of
        Just handler -> do
            newEnv <- handler env msg
            case isClientReady (Env.client newEnv) of
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
    pingTimeout = getConfigInt (Env.config env) "DEFAULT" "ping_timeout"
    client = Env.client env

handleLine :: Env.Env -> IO Env.Env
handleLine env = do
    line <- hGetLine handle
    let msg = parseMessage line
        cmd = command msg
    case M.lookup cmd (Env.handlers env) of
        Just h  -> modifyMVar sharedM process
          where process s = do
                    newEnv <- h locEnv msg
                    let newLocal    = Env.local newEnv
                        newClient   = Env.client newEnv
                        Just nick   = Client.nick newClient
                        nickUpper   = map toUpper nick
                        newClients  = IM.insert uid newClient (Env.clients s)
                        newUids     = case IM.lookup uid (Env.clients s) of
                            Just Client.Client {Client.nick=Just oldNick}
                                -> M.insert nickUpper uid $ M.delete (map toUpper oldNick) (Env.uids s)
                            _   -> M.insert nickUpper uid (Env.uids s)
                    return (newLocal {Env.clients=newClients, Env.uids=newUids}, newEnv)
                  where locEnv = env {Env.local=s}
        Nothing -> do
            sendNumeric env numERR_UNKNOWNCOMMAND [cmd, "Unknown command"]
            return env
  where
    Just sharedM    = Env.shared env
    client          = Env.client env
    Just handle     = Client.handle client -- force thread crash if handle is Nothing
    Just uid        = Client.uid client

toMicro :: Num a => a -> a
toMicro = (*1000000)

firstAvailable :: (Eq a, Enum a, Num a) => [a] -> a
firstAvailable = f 1
  where
    f n (x:xs)
        | n == x    = f (succ n) xs
        | otherwise = n
    f n _ = n
