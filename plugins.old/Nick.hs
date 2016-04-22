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
import Control.Monad ((>=>))
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
    | Client.registered (Env.client env) = do
        let a = if canChangeNick env nick
                then NamedAction "NickChange" (aSend >=> aChange)
                else GenericAction aInUse
        env {Env.actions=a:Env.actions env}
    | otherwise = do
        let a = if canChangeNick env nick
                then NamedAction "NickChange" aChange
                else GenericAction aInUse
        env {Env.actions=a:Env.actions env}
  where
    aSend e = do
        sendUniqCommon (Env.local e) cli $ ':' : show (clientToMask cli) ++ " NICK :" ++ nick
        return e
      where cli = Env.client e
    aChange e = return e {Env.client=(Env.client e) {Client.nick=Just nick}}
    aInUse e = sendNumeric e numERR_NICKNAMEINUSE [nick, "Nickname is already in use"] >> return e
nick env _ = env {Env.actions=a:Env.actions env}
  where a = GenericAction $ \e -> sendNumeric e numERR_NONICKNAMEGIVEN ["No nickname given"] >> return e

canChangeNick :: Env.Env -> String -> Bool
canChangeNick env newNick = do
    (newNickUpper == map toUpper nick && newNick /= nick) || M.notMember newNickUpper (Env.uids local)
  where
    newNickUpper = map toUpper newNick
    local = Env.local env
    client = Env.client env
    nick = fromMaybe "" (Client.nick client)
