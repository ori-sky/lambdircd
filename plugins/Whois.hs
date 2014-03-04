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
import Control.Concurrent.STM
import IRC.Message
import IRC.Numeric
import IRC.Server.Client (isClientRegistered)
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin {handlers=[("WHOIS", whois)]}

whois :: CommandHandler
{-
whois env (Message _ _ (server:_:_))
    | isClientRegistered client = do
        sendNumeric env (Numeric 402) [server, "No such server"]
        return env
    | otherwise = return env
  where client = Env.client env
-}
whois env (Message pfx cmd (_:target:_)) = whois env (Message pfx cmd [target])
whois env (Message _ _ (target:[]))
    | isClientRegistered client = do
        shared <- atomically $ readTVar sharedT
        if M.member targetUpper (Env.uids shared)
            then do
                let targetClient = Env.clients shared IM.! (Env.uids shared M.! targetUpper)
                    Just nick = Client.nick targetClient
                    Just user = Client.user targetClient
                    Just host = Client.host targetClient
                    Just real = Client.realName targetClient
                sendNumeric env (Numeric 311) [nick, user, host, "*", real]
                sendNumeric env (Numeric 318) [nick, "End of /WHOIS list"]
            else sendNumeric env (Numeric 401) [target, "No such nick"]
        return env
    | otherwise = return env
  where
    targetUpper = map toUpper target
    Just sharedT = Env.shared env
    client = Env.client env
whois env _
    | isClientRegistered client = do
        sendNumeric env (Numeric 460) ["WHOIS", "Not enough parameters"]
        return env
    | otherwise = return env
  where client = Env.client env
