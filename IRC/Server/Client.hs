module IRC.Server.Client where

import Data.Maybe
import System.IO

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
  where Just handle' = handle client
