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

module Nick where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import IRC.Action
import IRC.Message
import IRC.Numeric
import IRC.Server.Client.Helper
import IRC.Server.Channel.Helper
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin {handlers = [CommandHandler "NICK" nick]}

nick :: CommandHSpec
nick env (Message _ _ (nick:_))
    | Client.registered client = if canChangeNick env nick
        then do
            let a = NamedAction "Nick.change" $ sendUniqCommon local client $ ':':mask ++ " NICK :" ++ nick
            env {Env.actions=a:Env.actions env, Env.client=client {Client.nick=Just nick}}
        else do
            let a = GenericAction $ sendNumeric env numERR_NICKNAMEINUSE [nick, "Nickname is already in use"]
            env {Env.actions=a:Env.actions env}
    | otherwise = if canChangeNick env nick
        then env {Env.client=client {Client.nick=Just nick}}
        else do
            let a = GenericAction $ sendNumeric env numERR_NICKNAMEINUSE [nick, "Nickname is already in use"]
            env {Env.actions=a:Env.actions env}
  where
    local  = Env.local env
    client = Env.client env
    mask   = show (clientToMask client)
nick env _ = env {Env.actions=a:Env.actions env}
  where a = GenericAction $ sendNumeric env numERR_NONICKNAMEGIVEN ["No nickname given"]

canChangeNick :: Env.Env -> String -> Bool
canChangeNick env newNick = do
    (newNickUpper == map toUpper nick && newNick /= nick) || M.notMember newNickUpper (Env.uids local)
  where
    newNickUpper = map toUpper newNick
    local = Env.local env
    client = Env.client env
    nick = fromMaybe "" (Client.nick client)
