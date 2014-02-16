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

toMicro :: Num a => a -> a
toMicro = (*1000000)

data Options = Options
    { port              :: Int
    , connectTimeout    :: Int
    , pingTimeout       :: Int
    }

defaultOptions :: Options
defaultOptions = Options
    { port              = 6667
    , connectTimeout    = 20
    , pingTimeout       = 240
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
