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
import Control.Concurrent.STM
import IRC.Message
import IRC.Numeric
import IRC.Server.Client (isClientRegistered, clientToMask, sendClient)
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin
    { handlers =
        [ ("PRIVMSG", privmsg)
        ]
    }

privmsg :: CommandHandler
privmsg env (Message _ _ (target:text:_))
    | isClientRegistered client = do
        shared <- atomically $ readTVar sharedT
        let targetClient = Env.clients shared IM.! (Env.uids shared M.! targetUpper)
        let msg = ':' : show (clientToMask client) ++ " PRIVMSG " ++ target ++ " :" ++ text
        if M.member targetUpper (Env.uids shared)
            then sendClient targetClient msg
            else sendNumeric env (Numeric 401) [target, "No such nick/channel"]
        return env
    | otherwise = return env
  where
    targetUpper = map toUpper target
    Just sharedT = Env.shared env
    client = Env.client env
privmsg env (Message _ _ (_:[]))
    | isClientRegistered client = do
        sendNumeric env (Numeric 412) ["No text to send"]
        return env
    | otherwise = return env
  where
    client = Env.client env
privmsg env _
    | isClientRegistered client = do
        sendNumeric env (Numeric 411) ["No recipient given (PRIVMSG)"]
        return env
    | otherwise = return env
  where
    client = Env.client env
