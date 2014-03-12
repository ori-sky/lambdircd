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

module Nick where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import IRC.Message
import IRC.Numeric
import IRC.Server.Client (sendClient)
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin
    { handlers =
        [ ("NICK", nick)
        ]
    }

nick :: CommandHandler
nick env (Message _ _ (nick:_))
    | Client.registered client = do
        sendClient client $ (':':oldNick) ++ " NICK :" ++ nick
        tryChangeNick env nick
    | otherwise = tryChangeNick env nick
  where
    client = Env.client env
    Just oldNick = Client.nick client
nick env _ = do
    sendNumeric env numERR_NONICKNAMEGIVEN ["No nickname given"]
    return env

tryChangeNick :: Env.Env -> String -> IO Env.Env
tryChangeNick env newNick = do
    if (newNickUpper == map toUpper nick && newNick /= nick) || M.notMember newNickUpper (Env.uids local)
        then return env {Env.client=client {Client.nick=Just newNick}}
        else do
            sendNumeric env numERR_NICKNAMEINUSE [newNick, "Nickname is already in use"]
            return env
  where
    newNickUpper = map toUpper newNick
    local = Env.local env
    client = Env.client env
    nick = fromMaybe "" (Client.nick client)
