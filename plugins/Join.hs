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

module Join where

import qualified Data.Map as M
import IRC.Message
import IRC.Numeric
import IRC.Server.Client (sendClient)
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Options as Opts
import IRC.Server.Environment (whenRegistered)
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin {handlers=[("JOIN", join)]}

join :: CommandHandler
join env (Message _ _ (('#':cs):_)) = whenRegistered env $ if notElem chan channels
    then if length channels < maxChans
        then do
            let newChans = if M.member chan locChans
                then M.adjust (\xs -> uid:xs) chan locChans
                else M.insert chan [uid] locChans
            sendNumeric env (Numeric 0) [show newChans]
            sendClient client $ ":" ++ nick ++ " JOIN " ++ chan
            sendNumeric env numRPL_NAMREPLY ["=", chan, nick]
            sendNumeric env numRPL_ENDOFNAMES [chan, "End of /NAMES list"]
            return env
                { Env.client=client {Client.channels=chan:channels}
                , Env.local=local {Env.channels=newChans}
                }
        else do
            sendNumeric env numERR_TOOMANYCHANNELS [chan, "You have joined too many channels"]
            return env
    else do
        sendNumeric env numERR_USERONCHANNEL [nick, chan, "is already on channel"]
        return env
  where
    chan = '#' : cs
    Opts.Options {Opts.maxChannels=maxChans} = Env.options env
    local = Env.local env
    locChans = Env.channels local
    client = Env.client env
    Just nick = Client.nick client
    Just uid = Client.uid client
    channels = Client.channels client
join env (Message _ _ (chan:_)) = whenRegistered env $ do
    sendNumeric env numERR_BADCHANNAME [chan, "Illegal channel name"]
    return env
join env _ = whenRegistered env $ do
    sendNumeric env numERR_NEEDMOREPARAMS ["JOIN", "Not enough parameters"]
    return env
