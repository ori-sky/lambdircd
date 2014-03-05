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

module Ping where

import IRC.Message (Message(..))
import IRC.Numeric
import IRC.Server.Client as Client (sendClient)
import qualified IRC.Server.Environment as Env (client)
import Plugin

plugin = defaultPlugin
    { handlers =
        [ ("PING", ping)
        ]
    }

ping :: CommandHandler
ping env (Message _ _ (server1:_)) = do
    sendClient client $ ":lambdircd PONG lambdircd :" ++ server1
    return env
  where client = Env.client env
ping env _ = do
    sendNumeric env numERR_NEEDMOREPARAMS ["PING", "Not enough parameters"]
    return env
  where
