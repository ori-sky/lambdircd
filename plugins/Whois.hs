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

module Whois where

import Data.Char (toUpper)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import IRC.Message
import IRC.Numeric
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin {handlers=[("WHOIS", whois)]}

whois :: CommandHandler
{-
whois env (Message _ _ (server:_:_))
    | isClientRegistered client = do
        sendNumeric env numERR_NOSUCHSERVER [server, "No such server"]
        return env
    | otherwise = return env
  where client = Env.client env
-}
whois env (Message pfx cmd (_:target:_)) = whois env (Message pfx cmd [target])
whois env (Message _ _ (target:[]))
    | Client.registered client = do
        if M.member targetUpper (Env.uids local)
            then do
                let targetClient = Env.clients local IM.! (Env.uids local M.! targetUpper)
                    Just nick = Client.nick targetClient
                    Just user = Client.user targetClient
                    Just host = Client.host targetClient
                    Just real = Client.realName targetClient
                sendNumeric env numRPL_WHOISUSER [nick, user, host, "*", real]
                sendNumeric env numRPL_ENDOFWHOIS [nick, "End of /WHOIS list"]
            else sendNumeric env numERR_NOSUCHNICK [target, "No such nick"]
        return env
    | otherwise = return env
  where
    targetUpper = map toUpper target
    local = Env.local env
    client = Env.client env
whois env _
    | Client.registered client = do
        sendNumeric env numERR_NEEDMOREPARAMS ["WHOIS", "Not enough parameters"]
        return env
    | otherwise = return env
  where client = Env.client env
