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
privmsg env (Message _ _ (chan@('#':_):text:_)) = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where
    locChans = Env.channels (Env.local env)
    channels = Client.channels (Env.client env)
    aMsg e = do
        sendChannelOthersFromClient (Env.client e) e (Env.channels l M.! chan) $ "PRIVMSG " ++ chan ++ " :" ++ text
        return e
      where l = Env.local e
    aCannotSend e = sendNumeric env numERR_CANNOTSENDTOCHAN [chan, "Cannot send to channel"] >> return e
    aNoSuch e = sendNumeric env numERR_NOSUCHCHANNEL [chan, "No such channel"] >> return e
    a = if M.member chan locChans
        then if elem chan channels -- TODO: move into CModeN transform handler and define ChannelAction or something
            then NamedAction "PrivmsgChan" aMsg
            else GenericAction aCannotSend
        else GenericAction aNoSuch
privmsg env (Message _ _ (target:text:_)) = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where
    local = Env.local env
    targetUpper = map toUpper target
    targetCli = Env.clients local IM.! (Env.uids local M.! targetUpper)
    aMsg e = do
        sendClientFrom (show $ clientToMask (Env.client e)) targetCli $ "PRIVMSG " ++ target ++ " :" ++ text
        return e
    aNoSuch e = sendNumeric env numERR_NOSUCHNICK [target, "No such nick"] >> return e
    a = if M.member targetUpper (Env.uids local)
        then if Client.registered targetCli
            then NamedAction "PrivmsgNick" aMsg
            else GenericAction aNoSuch
        else GenericAction aNoSuch
privmsg env (Message _ _ (_:[])) = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_NOTEXTTOSEND ["No text to send"] >> return e
privmsg env _ = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_NORECIPIENT ["No recipient given (PRIVMSG)"] >> return e
