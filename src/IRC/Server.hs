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
                    --loopClient (env {Env.client=client}) False
                    return ()
                  where
                    client = Env.client env
                    Just nick = Client.nick client
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
            case Client.isClientRegistered (Env.client newEnv) of
                True    -> return newEnv
                False   -> registerClient env
        Nothing -> do
            putStrLn $ "no handler for `"++(command msg)++"`"
            registerClient env
  where
    Just handle = Client.handle (Env.client env)

{-
loopClient :: Options -> MessageHandler -> Client -> Bool -> IO ()
loopClient opts f client pinged = do
    maybeClient <- timeout
        $> toMicro (pingTimeout opts)
        $> handleLine opts f client
    case maybeClient of
        Nothing         -> case pinged of
            True            -> return ()
            False           -> sendClient client "PING :lambdaircd"
                               >> loopClient opts f client True
        Just client'    -> loopClient opts f client' False

handleLine :: Options -> MessageHandler -> Client -> IO Client
handleLine opts f client = do
    line <- hGetLine handle'
    f opts client (parseMessage line)
  where Just handle' = handle client
-}

toMicro :: Num a => a -> a
toMicro = (*1000000)
