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

module CModeNoExternal where

import qualified Data.Map as M
import IRC.Numeric
import IRC.Action
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Channel as Chan
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin {handlers=[TransformHandler trans]}

trans :: TransformHSpec
trans env = env {Env.actions=map f (Env.actions env)}
  where
    channels = Client.channels (Env.client env)
    f a@(ChanAction "Privmsg" chanName _) = if notElem 'n' (Chan.modes chan) || elem chanName channels
        then a
        else GenericAction $ \e -> sendNumeric e numERR_CANNOTSENDTOCHAN [chanName, "Cannot send to channel"]
            >> return e
      where chan = Env.channels (Env.local env) M.! chanName
    f a = a
