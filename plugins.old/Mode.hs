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

module Mode where

import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import IRC.Message
import IRC.Numeric
import IRC.Action
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Channel as Chan
import IRC.Server.Client.Helper
import IRC.Server.Channel.Helper
import IRC.Server.Environment (whenRegistered)
import qualified IRC.Server.Environment as Env
import Config
import Plugin

plugin = defaultPlugin {handlers=[CommandHandler "MODE" mode]}

mode :: CommandHSpec
mode env (Message _ _ (chan@('#':_):xs)) = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where
    locChans = Env.channels (Env.local env)
    aMsg e = do
        sendChannelOthersFromClient (Env.client e) e (Env.channels l M.! chan) $ "PRIVMSG " ++ chan ++ " :" ++ text
        return e
      where l = Env.local e
    aNoSuch e = sendNumeric e numERR_NOSUCHCHANNEL [chan, "No such channel"] >> return e
    a = if M.member chan locChans
        then ChanAction "Mode" chan aMsg
        else GenericAction aNoSuch
mode env (Message _ _ (chan:_)) = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_BADCHANNAME [chan, "Illegal channel name (no umodes yet)"]
            >> return e
mode env _ = whenRegistered env $ env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_NEEDMOREPARAMS ["MODE", "Not enough parameters"] >> return e
