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
import Control.Concurrent.STM
import System.IO
import System.IO.Error
import System.Timeout
import Network
import Network.Socket hiding (accept)
import Network.SocketServer
import IRC.Message
import IRC.Server.Client (isClientRegistered, sendClient)
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Options as Opts
import qualified IRC.Server.Environment as Env
import qualified Plugin as P
import Plugin.Load

serveIRC2 :: Env.Env -> IO ()
serveIRC2 env = withSocketsDo $ do
    sock <- listenOn (PortNumber port)
    setSocketOption sock ReuseAddr 1
    acceptLoop env sock
  where Env.Env {Env.options=Opts.Options {Opts.port=port}} = env

acceptLoop :: Env.Env -> Socket -> IO ()
acceptLoop env sock = do
    (handle, hostname, _) <- accept sock
    acceptLoop env sock

serveIRC :: Env.Env -> IO ()
serveIRC baseEnv = do
    plugins <- mapM loadPlugin pluginNames
    sharedT <- atomically $ newTVar Env.defaultShared
    let handlers = M.unions $ map (M.fromList.P.handlers) (catMaybes plugins)
    let env = baseEnv {Env.handlers=handlers, Env.shared=Just sharedT}

    serveTCPforever ((simpleTCPOptions (fromIntegral port)) {reuse=True})
        $ (threadedHandler.handleHandler) (\handle _ _ -> do
            hSetBuffering handle NoBuffering
            hSetNewlineMode handle universalNewlineMode
            hSetEncoding handle utf8

            shared <- atomically $ readTVar sharedT
            let uid = if M.null (Env.clients shared) then 1
                else fst (M.findMax (Env.clients shared)) + 1

            tryIOError $ do
                maybeEnv <- timeout (toMicro connectTimeout) $ registerClient env
                    { Env.client=Client.defaultClient
                        { Client.handle = Just handle
                        , Client.uid    = Just uid
                        }
                    }
                case maybeEnv of
                    Just newEnv -> do
                        sendClient client $ ":lambdircd 001 "++nick++" :Welcome to lambdircd "++nick
                        loopClient newEnv False
                      where
                        client = Env.client newEnv
                        nick = fromMaybe "*" (Client.nick client)
                    _ -> return ()
            atomically $ do
                shared <- readTVar sharedT
                let newClients = M.delete uid (Env.clients shared)
                writeTVar sharedT shared {Env.clients=newClients}
        )
  where
    Env.Env
        { Env.options = Opts.Options
            { Opts.plugins = pluginNames
            , Opts.port = port
            , Opts.connectTimeout = connectTimeout
            }
        } = baseEnv

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
    atomically $ do
        shared <- readTVar sharedT
        let newClients = M.insert uid client (Env.clients shared)
        writeTVar sharedT shared {Env.clients=newClients}
    line <- hGetLine handle
    let msg = parseMessage line
    case M.lookup (command msg) (Env.handlers env) of
        Just handler -> do
            let newEnv = handler env msg
            newEnv
        Nothing -> do
            sendClient client $ ":lambdircd 431 " ++ nick ++ (' ':(command msg)) ++ " :Unknown command"
            handleLine env
  where
    Just sharedT = Env.shared env
    client = Env.client env
    Just uid = Client.uid client
    Just handle = Client.handle client
    Just nick = Client.nick client
    -- force thread crash if Nothing which should never happen here

toMicro :: Num a => a -> a
toMicro = (*1000000)
