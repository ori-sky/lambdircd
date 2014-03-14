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
import Data.List (sort, delete)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad (forM, (>=>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.IO
import System.IO.Error
import System.Timeout
import Network hiding (accept)
import Network.Socket
import IRC.Message
import IRC.Numeric
import IRC.Action
import IRC.Server.Client (defaultClient)
import IRC.Server.Client.Helper
import IRC.Server.Channel.Helper
import qualified IRC.Server.Client as Cli
import qualified IRC.Server.Channel as Chan
import qualified IRC.Server.Environment as Env
import Plugin hiding (name, handlers)
import qualified Plugin as P
import Config
import Plugin.Load

serveIRC :: Env.Env -> IO ()
serveIRC env = withSocketsDo $ do
    plugins <- forM pluginNames $ \p -> putStrLn ("Loading plugin `" ++ p ++ "`") >> loadPlugin p
    putStrLn "Finished loading plugins"
    sharedM <- newMVar Env.defaultShared
    let handlers = concat $ map P.handlers $ catMaybes plugins
        commandHandlers = M.fromList [(k,v) | CommandHandler k v <- handlers]
        transformHandlers = [v | TransformHandler v <- handlers]
        newEnv = env
            { Env.shared = Just sharedM
            , Env.commandHandlers = commandHandlers
            , Env.transformHandlers = transformHandlers
            }
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    {- 6 = TCP option, 9 = defer accept
     - only supported on GNU systems
     -}
    tryIOError $ setSocketOption sock (CustomSockOpt (6, 9)) 30
        >> putStrLn "Using deferred accept for connections"
    bindSocket sock $ SockAddrInet (fromIntegral port) iNADDR_ANY
    listen sock 5
    putStrLn $ "Listening on port " ++ show port
    acceptLoop sock newEnv
  where
    cp = Env.config env
    pluginNames = words $ getConfigString cp "plugins" "load"
    port = getConfigInt cp "listen" "port"

acceptLoop :: Socket -> Env.Env-> IO ()
acceptLoop sock env = do
    tryIOError $ do
        (clientSock, sockAddr) <- accept sock
        handle <- socketToHandle clientSock ReadWriteMode
        hSetNewlineMode handle universalNewlineMode
        hSetBuffering handle LineBuffering
        hSetEncoding handle utf8
        let client = defaultClient {Cli.handle=Just handle}
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
        let client = baseClient {Cli.host=Just host}
        maybeEnv <- timeout (toMicro connectTimeout) $ registerClient env {Env.client=client, Env.local=shared}
        case maybeEnv of
            Just newEnv -> do
                newShared <- takeMVar sharedM
                let clients     = Env.clients newShared
                    uids        = Env.uids newShared
                    uid         = firstAvailable $ sort (IM.keys clients)
                    regClient   = (Env.client newEnv) {Cli.uid=Just uid, Cli.registered=True}
                    regEnv      = newEnv {Env.client=regClient}
                    newClients  = IM.insert uid regClient clients
                    newUids     = M.insert (map toUpper nick) uid uids
                    Just nick   = Cli.nick regClient
                    cleanup s   = do
                        sendUniqCommonOthers s finClient $ ':' : show (clientToMask finClient) ++ " QUIT :" ++
                            case maybeReason of
                                Just reason -> reason
                                Nothing     -> "Client Quit"
                        return $ case IM.lookup uid (Env.clients s) of
                            Just (Cli.Client {Cli.nick=Just n})
                                -> s {Env.clients=finClients, Env.uids=finUids, Env.channels=finChans}
                              where finUids = M.delete (map toUpper n) (Env.uids s)
                            _   -> s {Env.clients=finClients, Env.channels=finChans}
                      where
                        finClients = IM.delete uid (Env.clients s)
                        finClient = (Env.clients s) IM.! uid
                        maybeReason = Cli.quitReason finClient
                        finChans = M.mapWithKey f (Env.channels s)
                        f name channel = if elem name (Cli.channels finClient)
                            then channel {Chan.uids=delete uid (Chan.uids channel)}
                            else channel
                if M.notMember (map toUpper nick) uids
                    then do
                        putMVar sharedM newShared {Env.clients=newClients, Env.uids=newUids}
                        sendNumeric regEnv numRPL_WELCOME   ["Welcome to "++networkName++" " ++ nick]
                        sendNumeric regEnv numRPL_YOURHOST  ["Your host is "++serverName++", running lambdircd"]
                        sendNumeric regEnv numRPL_CREATED   ["This server was created (Just now)"]
                        sendNumeric regEnv numRPL_MOTDSTART ["- "++serverName++" Message of the Day -"]
                        sendNumeric regEnv numRPL_MOTD      ["- Welcome to lambdircd"]
                        sendNumeric regEnv numRPL_ENDOFMOTD ["End of /MOTD command"]
                        sendNumeric regEnv (Numeric 0) ["Your host is `" ++ host ++ "`"]
                        tryIOError $ loopClient regEnv False
                        modifyMVar_ sharedM cleanup
                    else do
                        putMVar sharedM newShared
                        sendNumeric regEnv numERR_NICKCOLLISION [nick, "Nickname collision KILL"]
                        sendClient regClient $ "ERROR :Closing Link " ++ host ++ " (Nickname collision KILL)"
            Nothing -> sendClient client $ "ERROR :Closing Link " ++ host ++ " (Connection timed out)"
    tryIOError $ hClose handle
    return ()
  where
    Just sharedM = Env.shared env
    cp = Env.config env
    serverName = getConfigString cp "info" "name"
    networkName = getConfigString cp "info" "network"
    connectTimeout = getConfigInt cp "client" "connect_timeout"
    baseClient = Env.client env
    Just handle = Cli.handle baseClient

registerClient :: Env.Env -> IO Env.Env
registerClient env = do
    line <- hGetLine handle
    let msg = parseMessage line
    case M.lookup (command msg) (Env.commandHandlers env) of
        Just commandH -> do
            let newEnv = commandH env {Env.actions=[]} msg
                finEnv = foldr (.) id (Env.transformHandlers env) newEnv
                fs = map actionIO (Env.actions finEnv)
            retEnv <- foldr (>=>) return fs finEnv
            case isClientReady (Env.client retEnv) of
                True    -> return retEnv
                False   -> registerClient retEnv
        Nothing -> registerClient env
  where Just handle = Cli.handle (Env.client env)

loopClient :: Env.Env -> Bool -> IO ()
loopClient env pinged = do
    maybeEnv <- timeout (toMicro pingTimeout) (handleLine env)
    case maybeEnv of
        Just newEnv -> loopClient newEnv False
        Nothing     -> case pinged of
            True        -> return ()
            False       -> sendClient client ("PING :"++serverName) >> loopClient env True
  where
    cp = Env.config env
    serverName = getConfigString cp "info" "name"
    pingTimeout = getConfigInt cp "client" "ping_timeout"
    client = Env.client env

handleLine :: Env.Env -> IO Env.Env
handleLine env = do
    line <- hGetLine handle
    let msg = parseMessage line
        cmd = command msg
    case M.lookup cmd (Env.commandHandlers env) of
        Just commandH  -> modifyMVar sharedM process
          where process s = do
                    let newEnv = commandH locEnv {Env.actions=[]} msg
                        finEnv = foldr (.) id (Env.transformHandlers env) newEnv
                        fs = map actionIO (Env.actions finEnv)
                    retEnv <- foldr (>=>) return fs finEnv
                    let newLocal    = Env.local retEnv
                        newClient   = Env.client retEnv
                        Just nick   = Cli.nick newClient
                        nickUpper   = map toUpper nick
                        newClients  = IM.insert uid newClient (Env.clients s)
                        newUids     = case IM.lookup uid (Env.clients s) of
                            Just Cli.Client {Cli.nick=Just oldNick}
                                -> M.insert nickUpper uid $ M.delete (map toUpper oldNick) (Env.uids s)
                            _   -> M.insert nickUpper uid (Env.uids s)
                    return (newLocal {Env.clients=newClients, Env.uids=newUids}, retEnv)
                  where locEnv = env {Env.local=s}
        Nothing -> do
            sendNumeric env numERR_UNKNOWNCOMMAND [cmd, "Unknown command"]
            return env
  where
    Just sharedM    = Env.shared env
    client          = Env.client env
    Just handle     = Cli.handle client -- force thread crash if handle is Nothing
    Just uid        = Cli.uid client

toMicro :: Num a => a -> a
toMicro = (*1000000)

firstAvailable :: (Eq a, Enum a, Num a) => [a] -> a
firstAvailable = f 1
  where
    f n (x:xs)
        | n == x    = f (succ n) xs
        | otherwise = n
    f n _ = n
