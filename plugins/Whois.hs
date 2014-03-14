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
import Control.Monad (unless)
import IRC.Message
import IRC.Numeric
import IRC.Action
import qualified IRC.Server.Client as Client
import IRC.Server.Environment (whenRegistered)
import qualified IRC.Server.Environment as Env
import Config
import Plugin

plugin = defaultPlugin {handlers=[CommandHandler "WHOIS" whois]}

whois :: CommandHSpec
{-
whois env (Message _ _ (server:_:_))
    | isClientRegistered client = do
        sendNumeric env numERR_NOSUCHSERVER [server, "No such server"]
        return env
    | otherwise = return env
  where client = Env.client env
-}
whois env (Message pfx cmd (_:target:_)) = whois env (Message pfx cmd [target])
whois env (Message _ _ (target:[])) = whenRegistered env $ do
    if M.member targetUpper (Env.uids local)
        then do
            let targetClient = Env.clients local IM.! (Env.uids local M.! targetUpper)
                Just nick = Client.nick targetClient
                Just user = Client.user targetClient
                Just host = Client.host targetClient
                Just real = Client.realName targetClient
                channels = Client.channels targetClient
                as =
                  [ NamedAction "WhoisUser"    $ \e -> sendNumeric e numRPL_WHOISUSER [nick, user, host, "*", real]
                        >> return e
                  , NamedAction "WhoisChans"   $ \e -> unless (null channels)
                        (sendNumeric e numRPL_WHOISCHANNELS [nick, unwords channels]) >> return e
                  , NamedAction "WhoisServer"  $ \e -> sendNumeric e numRPL_WHOISSERVER [nick, serverName, serverDesc]
                        >> return e
                  , GenericAction $ \e -> sendNumeric e numRPL_ENDOFWHOIS [nick, "End of /WHOIS list"] >> return e
                  ]
            env {Env.actions=as++Env.actions env}
        else do
            let a = GenericAction $ \e -> sendNumeric e numERR_NOSUCHNICK [target, "No such nick"] >> return e
            env {Env.actions=a:Env.actions env}
  where
    cp = Env.config env
    serverName = getConfigString cp "info" "name"
    serverDesc = getConfigString cp "info" "description"
    targetUpper = map toUpper target
    local = Env.local env
whois env _ = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_NEEDMOREPARAMS ["WHOIS", "Not enough parameters"]
            >> return e
