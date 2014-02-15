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

-- left associative application
($>) :: (a -> b) -> a -> b
f $> x = f x
infixl 0 $>

data Prefix = StringPrefix String
            | MaskPrefix
                { prefixNick  :: String
                , prefixUser  :: String
                , prefixHost  :: String
                }

instance Show Prefix where
    show (StringPrefix s) = s
    show (MaskPrefix n u h) = n ++ ('!':u) ++ ('@':h)

data Message = Message
    { messagePrefix     :: Maybe Prefix
    , messageCommand    :: String
    , messageParams     :: [String]
    } deriving (Show)

data Client = Client
    { clientHandle      :: Maybe Handle
    , clientNick        :: Maybe String
    , clientUser        :: Maybe String
    , clientRealName    :: Maybe String
    }
clientDefault = Client
    { clientHandle      = Nothing
    , clientNick        = Nothing
    , clientUser        = Nothing
    , clientRealName    = Nothing
    }

ircParams :: String -> [String]
ircParams "" = []
ircParams (':':xs) = [xs]
ircParams s = x : (ircParams.(drop 1) $ xs)
  where (x, xs) = break isSpace s

parseMessage :: String -> Message
parseMessage (':':s) = Message
    (Just (StringPrefix $ head.words $ s))
    (head.tail.ircParams $ s)
    (tail.tail.ircParams $ s)
parseMessage s = Message
    Nothing
    (head.ircParams $ s)
    (tail.ircParams $ s)

main :: IO ()
main = do
    serveTCPforever (simpleTCPOptions 6667) {reuse = True}
        $ threadedHandler $ handleHandler
        (\handle _ _ -> do
            hSetBuffering handle NoBuffering
            hSetNewlineMode handle universalNewlineMode
            hSetEncoding handle utf8

            let client = clientDefault {clientHandle=Just handle}

            putStrLn "connected"
            mClient <- timeout 15000000 $ clientHandler client
            case mClient of
                Nothing     -> return ()
                _           -> clientLoop (fromJust mClient)
            putStrLn "disconnected"
        )

clientLoop :: Client -> IO ()
clientLoop = clientLoop' False
clientLoop' :: Bool -> Client -> IO ()
clientLoop' pinged client = do
    mClient <- timeout 90000000 $ clientHandler client
    case mClient of
        Nothing     -> case pinged of
            True        -> return ()
            False       -> clientLoop' True client
        _           -> clientLoop (fromJust mClient)

clientHandler :: Client -> IO Client
clientHandler client = do
    line <- hGetLine $ fromJust (clientHandle client)
    client <- messageHandler client (parseMessage line)
    putStrLn $ show (clientNick client)
    clientHandler client

messageHandler :: Client -> Message -> IO Client
messageHandler client (Message {messageCommand="NICK", messageParams=(nick:_)})
    = return client {clientNick=Just nick}
messageHandler client message = putStrLn (show message) >> return client
