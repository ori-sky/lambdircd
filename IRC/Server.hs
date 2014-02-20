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
import System.IO
import System.Timeout
import Network.SocketServer
import LeftApplication
import qualified IRC as IRC
import IRC.Server.Options
import IRC.Server.Client
import IRC.Server.MessageHandler
import Plugin.Load

toMicro :: Num a => a -> a
toMicro = (*1000000)

serveIRC :: Options -> MessageHandler -> IO ()
serveIRC opts f = do
    plugins <- mapM loadPlugin (plugins opts)
    putStrLn $ show (length $ catMaybes plugins)
    serveTCPforever
        $> (simpleTCPOptions (port opts)) {reuse = True}
        $> (threadedHandler . handleHandler) (\handle _ _ -> do
            hSetBuffering handle NoBuffering
            hSetNewlineMode handle universalNewlineMode
            hSetEncoding handle utf8
            maybeClient <- timeout
                $> toMicro (connectTimeout opts)
                $> registerClient opts f (defaultClient {handle = Just handle})
            case maybeClient of
                Nothing         -> return ()
                Just client'    -> do
                    sendClient client' $ ":lambdircd 001 " ++ nick
                        ++ " :Welcome to the lambdircd Internet Relay Network " ++ nick
                    loopClient opts f client' False
                  where Just nick = IRC.Server.Client.nick client'
        )

registerClient :: Options -> MessageHandler -> Client -> IO Client
registerClient opts f client = do
    line <- hGetLine handle'
    newClient <- f opts client (IRC.parseMessage line)
    case isClientRegistered newClient of
        True    -> return newClient
        False   -> registerClient opts f newClient
  where Just handle' = handle client

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
    f opts client (IRC.parseMessage line)
  where Just handle' = handle client
