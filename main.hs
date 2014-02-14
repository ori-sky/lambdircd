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
                { nick  :: String
                , user  :: String
                , host  :: String
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
    { clientHandle      :: Handle
    , clientNick        :: Maybe String
    }

parseMessage :: String -> Message
parseMessage (':':s) = Message
    (Just (StringPrefix $ head.words $ s))
    (head.tail.words $ s)
    (tail.tail.words $ s)
parseMessage s = Message
    Nothing
    (head.words $ s)
    (tail.words $ s)

main :: IO ()
main = do
    serveTCPforever (simpleTCPOptions 6667) {reuse = True}
        $ threadedHandler $ handleHandler
        (\handle _ _ -> do
            hSetBuffering handle NoBuffering
            hSetNewlineMode handle universalNewlineMode
            hSetEncoding handle utf8

            let client = Client handle Nothing

            putStrLn "connected"
            result <- timeout 15000000 $ clientHandler client
            -- TODO: return if Nothing
            clientLoop client
            putStrLn "disconnected"
            return ()
        )

clientLoop :: Client -> IO ()
clientLoop = clientLoop' False
clientLoop' :: Bool -> Client -> IO ()
clientLoop' pinged client = do
    result <- timeout 90000000 $ clientHandler client
    case result of
        Nothing     -> case pinged of
            True        -> return ()
            False       -> clientLoop' True client
        Just False  -> return ()
        _           -> clientLoop client

clientHandler :: Client -> IO Bool
clientHandler client = do
    line <- hGetLine (clientHandle client)
    messageHandler client $ parseMessage line
    clientHandler client

messageHandler :: Client -> Message -> IO Bool
messageHandler _ message = putStrLn (show message) >> return True
