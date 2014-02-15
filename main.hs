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

import Data.Char
import Data.List
import Data.Maybe
import System.Environment (getArgs)
import System.IO
import System.Timeout
import Network
import Network.Socket
import Network.SocketServer
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import LeftApplication
import IRC

data Client = Client
    { clientHandle      :: Maybe Handle
    , clientNick        :: Maybe String
    , clientUser        :: Maybe String
    , clientRealName    :: Maybe String
    } deriving (Show)
clientDefault = Client
    { clientHandle      = Nothing
    , clientNick        = Nothing
    , clientUser        = Nothing
    , clientRealName    = Nothing
    }
clientRegistered :: Client -> Bool
clientRegistered client =
    isJust (clientNick client) &&
    isJust (clientUser client) &&
    isJust (clientRealName client)

ircParams :: String -> [String]
ircParams "" = []
ircParams (':':xs) = [xs]
ircParams s = x : (ircParams.(drop 1) $ xs)
  where (x, xs) = break isSpace s

parseMessage :: String -> Message
parseMessage "" = Message Nothing "" []
parseMessage (':':s) = Message
    (Just (StringPrefix $ head.words $ s))
    (head.tail.ircParams $ s)
    (tail.tail.ircParams $ s)
parseMessage s = Message
    Nothing
    (head.ircParams $ s)
    (tail.ircParams $ s)

clientSend :: Client -> String -> IO ()
clientSend client s = do
    putStrLn $ "-> " ++ s
    hPutStr (fromJust $ clientHandle client) $ s ++ "\r\n"

main :: IO ()
main = do
    serveTCPforever (simpleTCPOptions 6667) {reuse = True}
        $ threadedHandler $ handleHandler
        (\handle _ _ -> do
            hSetBuffering handle NoBuffering
            hSetNewlineMode handle universalNewlineMode
            hSetEncoding handle utf8

            mClient <- timeout 15000000 $ clientRegistration (clientDefault {clientHandle = Just handle})
            case mClient of
                Nothing -> return ()
                _       -> clientLoop $ fromJust mClient
        )

clientRegistration :: Client -> IO Client
clientRegistration client = do
    line <- hGetLine $ fromJust (clientHandle client)
    client <- messageHandler client (parseMessage line)
    case clientRegistered client of
        True    -> return client
        False   -> clientRegistration client

clientLoop' :: Bool -> Client -> IO ()
clientLoop' pinged client = do
    mClient <- timeout 90000000 $ lineHandler client
    case mClient of
        Nothing     -> case pinged of
            True        -> return ()
            False       -> clientSend client "PING : lambdircd" >> clientLoop' True client
        _           -> clientLoop' False (fromJust mClient)

clientLoop :: Client -> IO ()
clientLoop client = do
    clientSend client $ ":lambdircd 001 "++nick++" :Welcome to the lambdircd Internet Relay Network "++nick
    clientLoop' False client
  where nick = fromJust $ clientNick client

lineHandler :: Client -> IO Client
lineHandler client = do
    line <- hGetLine $ fromJust (clientHandle client)
    messageHandler client (parseMessage line)

messageHandler :: Client -> Message -> IO Client
messageHandler client message = putStrLn (show message) >> messageProcessor client message

messageProcessor :: Client -> Message -> IO Client
messageProcessor client (Message _ "PING" (server1:_)) = do
    clientSend client $ ":lambdircd PONG lambdircd :" ++ server1
    return client
messageProcessor client (Message _ "NICK" (nick:_)) = return client {clientNick = Just nick}
messageProcessor client (Message _ "USER" (user:mode:unused:realname:_)) =
    return client {clientUser = Just user, clientRealName = Just realname}
messageProcessor client (Message _ command _) = do
    clientSend client $ ":lambdircd 421 " ++ nick ++ (' ':command) ++ " :Unknown command"
    return client
  where nick = fromJust $ clientNick client
