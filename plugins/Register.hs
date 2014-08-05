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

module Register (plugin) where

import Data.Maybe (isJust)
import IRCD.Types
import IRCD.Helper
import Hoist

plugin :: Plugin
plugin = defaultPlugin {transformers=[Transformer register 200]}

register :: TransformerSpec
register action@(NickChangeAction (ClientSrc client) _ new _)
    | canRegister client {nick=Just new} = return [action, GenericAction io]
    | otherwise = return [action]
  where io = hoistState $ updateClientRegistered True (uid client)
register action@(UserChangeAction (ClientSrc client) _ new _)
    | canRegister client {user=Just new} = return [action, GenericAction io]
    | otherwise = return [action]
  where io = hoistState $ updateClientRegistered True (uid client)
register action@(RealNameChangeAction (ClientSrc client) _ new _)
    | canRegister client {realName=Just new} = return [action, GenericAction io]
    | otherwise = return [action]
  where io = hoistState $ updateClientRegistered True (uid client)
register action = return [action]

canRegister :: Client -> Bool
canRegister client
    | registered client = False
    | isJust (nick client) && isJust (user client) && isJust (realName client) = True
    | otherwise = False
