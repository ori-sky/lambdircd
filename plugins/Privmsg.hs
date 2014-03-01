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

import IRC.Message
import IRC.Numeric
import IRC.Server.Client (isClientRegistered)
import qualified IRC.Server.Client as Client
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
        putStrLn "TODO: PRIVMSG"
        return env
    | otherwise = return env
  where
    client = Env.client env
    Just nick = Client.nick client
privmsg env (Message _ _ (target:[]))
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
