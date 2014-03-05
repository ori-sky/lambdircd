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

module User where

import IRC.Message
import IRC.Numeric
import qualified IRC.Server.Client as Client
import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin
    { handlers =
        [ ("USER", user)
        ]
    }

user :: CommandHandler
user env (Message _ _ (user:_:_:realname:_))
    | Client.registered client = do
        sendNumeric env numERR_ALREADYREGISTERED ["You may not reregister"]
        return env
    | otherwise = return env {Env.client=client {Client.user=Just user, Client.realName=Just realname}}
  where
    client = Env.client env
user env _
    | Client.registered client = do
        sendNumeric env numERR_ALREADYREGISTERED ["You may not reregister"]
        return env
    | otherwise = do
        sendNumeric env numERR_NEEDMOREPARAMS ["USER", "Not enough parameters"]
        return env
  where
    client = Env.client env
