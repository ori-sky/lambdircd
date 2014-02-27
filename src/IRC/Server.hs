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
import System.IO
import System.Timeout
import Network.SocketServer
import IRC.Message
import qualified IRC.Server.Options as Opts
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Environment as Env
import qualified Plugin as P
import Plugin.Load

serveIRC :: Env.Env -> IO ()
serveIRC baseEnv = do
    plugins <- mapM loadPlugin pluginNames
    let handlers = M.unions $ map (M.fromList.P.handlers) (catMaybes plugins)
    let env = baseEnv {Env.handlers=handlers}

    serveTCPforever ((simpleTCPOptions port) {reuse=True})
        $ (threadedHandler.handleHandler) (\handle _ _ -> do
            hSetBuffering handle NoBuffering
            hSetNewlineMode handle universalNewlineMode
            hSetEncoding handle utf8
            maybeEnv <- timeout (toMicro connectTimeout)
                $ registerClient env {Env.client=Client.defaultClient {Client.handle=Just handle}}
            case maybeEnv of
                Just newEnv -> do
                    Client.sendClient client $ ":lambdircd 001 "++nick++" :Welcome to lambdircd "++nick
                    loopClient (newEnv {Env.client=client}) False
                  where
                    client = Env.client newEnv
                    nick = fromMaybe "*" (Client.nick client)
                _ -> return ()
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
            print (Env.client newEnv)
            case Client.isClientRegistered (Env.client newEnv) of
                True    -> return newEnv
                False   -> registerClient newEnv
        Nothing -> do
            putStrLn $ "no handler for `"++(command msg)++"`"
            registerClient env
  where Just handle = Client.handle (Env.client env)

loopClient :: Env.Env -> Bool -> IO ()
loopClient env pinged = do
    maybeEnv <- timeout (toMicro pingTimeout) (handleLine env)
    case maybeEnv of
        Just newEnv -> loopClient newEnv False
        Nothing     -> case pinged of
            True        -> return ()
            False       -> Client.sendClient client "PING :lambdircd"
                            >> loopClient env True
  where
    pingTimeout = Opts.pingTimeout (Env.options env)
    client = Env.client env

handleLine :: Env.Env -> IO Env.Env
handleLine env = do
    line <- hGetLine handle
    let msg = parseMessage line
    case M.lookup (command msg) (Env.handlers env) of
        Just handler -> handler env msg
        Nothing -> do
            putStrLn $ "no handler for `"++(command msg)++"`"
            handleLine env
  where Just handle = Client.handle (Env.client env)

toMicro :: Num a => a -> a
toMicro = (*1000000)
