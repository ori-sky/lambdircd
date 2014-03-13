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

module Part where

import Data.List
import qualified Data.Map as M
import IRC.Message
import IRC.Numeric
import IRC.Action
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Channel as Chan
import IRC.Server.Channel.Helper
import IRC.Server.Environment (whenRegistered)
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin {handlers = [CommandHandler "PART" part]}

part :: CommandHSpec
part env (Message _ _ (chan@('#':_):xs)) = whenRegistered env $ do
    let a = if M.member chan locChans
            then if elem chan channels
                then NamedAction "Part" aPart
                else GenericAction aNotOn
            else GenericAction aNoSuch
    env {Env.actions=a:Env.actions env}
  where
    locChans = Env.channels (Env.local env)
    channels = Client.channels (Env.client env)
    aPart = \e -> do
        let l   = Env.local e
            lcs = Env.channels l
            cli = Env.client e
            cs  = Client.channels (Env.client e)
            Just uid = Client.uid cli
            newChans = M.adjust (\c@(Chan.Channel {Chan.uids=us}) -> c {Chan.uids=delete uid us}) chan lcs
        sendChannelFromClient cli e (lcs M.! chan) $ "PART " ++ chan ++
            case xs of
                reason:_    -> ' ' : ':' : reason
                []          -> ""
        return env { Env.client = cli {Client.channels=delete chan cs}
                   , Env.local  = l {Env.channels=newChans}
                   }
    aNotOn = \e -> sendNumeric e numERR_NOTONCHANNEL [chan, "You're not on that channel"] >> return e
    aNoSuch = \e -> sendNumeric e numERR_NOSUCHCHANNEL [chan, "No such channel"] >> return e

part env (Message _ _ (chan:_)) = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_BADCHANNAME [chan, "Illegal channel name"] >> return e
part env _ = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_NEEDMOREPARAMS ["PART", "Not enough parameters"] >> return e
