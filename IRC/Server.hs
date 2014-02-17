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
( Options(..)
, defaultOptions
, MessageHandler
, serveIRC
, Client(..)
, defaultClient
, isClientRegistered
, sendClient
) where

import Data.Maybe
import System.IO
import System.Timeout
import Network.SocketServer
import LeftApplication
import qualified IRC as IRC
import Plugin

toMicro :: Num a => a -> a
toMicro = (*1000000)

data Options = Options
    { port              :: Int
    , connectTimeout    :: Int
    , pingTimeout       :: Int
    , plugins           :: [String]
    }

defaultOptions :: Options
defaultOptions = Options
    { port              = 6667
    , connectTimeout    = 20
    , pingTimeout       = 240
    , plugins           = []
    }

type MessageHandler = Options -> Client -> IRC.Message -> IO Client

serveIRC :: Options -> MessageHandler -> IO ()
serveIRC opts f = serveTCPforever
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
              where Just nick = IRC.Server.nick client'
    )

data Client = Client
    { handle      :: Maybe Handle
    , nick        :: Maybe String
    , user        :: Maybe String
    , realName    :: Maybe String
    } deriving (Show)

defaultClient :: Client
defaultClient = Client
    { handle      = Nothing
    , nick        = Nothing
    , user        = Nothing
    , realName    = Nothing
    }

isClientRegistered :: Client -> Bool
isClientRegistered client =
    isJust (nick client) &&
    isJust (user client) &&
    isJust (realName client)

sendClient :: Client -> String -> IO ()
sendClient client message = do
    putStrLn $ "-> " ++ message
    hPutStr handle' $ message ++ "\r\n"
  where Just handle' = IRC.Server.handle client

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
