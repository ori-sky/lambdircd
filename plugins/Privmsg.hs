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

module Privmsg where

import Data.Char (toUpper)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import IRC.Message
import IRC.Numeric
import IRC.Action
import IRC.Server.Client.Helper
import qualified IRC.Server.Client as Client
import IRC.Server.Channel.Helper
import IRC.Server.Environment (whenRegistered)
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin {handlers = [CommandHandler "PRIVMSG" privmsg]}

privmsg :: CommandHSpec
privmsg env (Message _ _ (chan@('#':_):text:_)) = whenRegistered env $ do
    let a = if M.member chan locChans
            then if elem chan channels
                then NamedAction "Privmsg.chan" $ sendChannelOthersFromClient client env (locChans M.! chan) $
                        "PRIVMSG " ++ chan ++ " :" ++ text
                else GenericAction $ sendNumeric env numERR_CANNOTSENDTOCHAN [chan, "Cannot send to channel"]
            else GenericAction $ sendNumeric env numERR_NOSUCHCHANNEL [chan, "No such channel"]
    env {Env.actions=a:Env.actions env}
  where
    local = Env.local env
    locChans = Env.channels local
    client = Env.client env
    channels = Client.channels client
privmsg env (Message _ _ (target:text:_)) = whenRegistered env $ do
    let a = if M.member targetUpper (Env.uids local)
            then do
                let targetClient = Env.clients local IM.! (Env.uids local M.! targetUpper)
                    msg = ':' : show (clientToMask client) ++ " PRIVMSG " ++ target ++ " :" ++ text
                if Client.registered targetClient
                    then NamedAction "Privmsg.nick" $ sendClient targetClient msg
                    else GenericAction $ sendNumeric env numERR_NOSUCHNICK [target, "No such nick"]
            else GenericAction $ sendNumeric env numERR_NOSUCHNICK [target, "No such nick"]
    env {Env.actions=a:Env.actions env}
  where
    targetUpper = map toUpper target
    local = Env.local env
    client = Env.client env
privmsg env (Message _ _ (_:[])) = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ sendNumeric env numERR_NOTEXTTOSEND ["No text to send"]
privmsg env _ = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ sendNumeric env numERR_NORECIPIENT ["No recipient given (PRIVMSG)"]
