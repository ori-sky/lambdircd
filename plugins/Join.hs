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

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import IRC.Message
import IRC.Numeric
import IRC.Action
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Channel as Chan
import IRC.Server.Channel.Helper
import IRC.Server.Environment (whenRegistered)
import qualified IRC.Server.Environment as Env
import Config
import Plugin

plugin = defaultPlugin {handlers=[CommandHandler "JOIN" join]}

join :: CommandHSpec
join env (Message _ _ (chan@('#':_):_)) = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where
    cp = Env.config env
    maxChans = getConfigInt cp "client" "max_channels"
    defChanModes = getConfigString cp "channel" "default_modes"
    channels = Client.channels (Env.client env)
    aJoin e = do
        sendChannelFromClient (Env.client e) e (newChans M.! chan) $ "JOIN " ++ chan
        sendNumeric e numRPL_NAMREPLY ["=", chan, unwords nicks]
        sendNumeric e numRPL_ENDOFNAMES [chan, "End of /NAMES list"]
        return e
            { Env.client = cli {Client.channels=chan:(Client.channels cli)}
            , Env.local  = l {Env.channels=newChans}
            }
      where
        l = Env.local e
        lcs = Env.channels l
        cli = Env.client e
        Just uid = Client.uid cli
        newChans = if M.member chan lcs
            then M.adjust (\c@(Chan.Channel {Chan.uids=us}) -> c {Chan.uids=uid:us}) chan lcs
            else M.insert chan (Chan.Channel chan [uid] defChanModes) lcs
        nicks = map (fromMaybe "*" . Client.nick . (Env.clients l IM.!)) $ Chan.uids (newChans M.! chan)
    aTooMany e = sendNumeric e numERR_TOOMANYCHANNELS [chan, "You have joined too many channels"] >> return e
    aAlready e = sendNumeric e numERR_USERONCHANNEL [nick, chan, "is already on channel"] >> return e
      where Just nick = Client.nick (Env.client e)
    a = if notElem chan channels
        then if length channels < maxChans
            then NamedAction "Join" aJoin
            else GenericAction aTooMany
        else GenericAction aAlready
join env (Message _ _ (chan:_)) = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_BADCHANNAME [chan, "Illegal channel name"] >> return e
join env _ = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_NEEDMOREPARAMS ["JOIN", "Not enough parameters"] >> return e
