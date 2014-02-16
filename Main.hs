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

import Data.Maybe
import IRC
import IRC.Server as IRCD

main :: IO ()
main = serveIRC defaultOptions messageHandler

messageHandler :: Options -> Client -> Message -> IO Client
messageHandler _ client message = putStrLn (show message) >> messageProcessor client message

messageProcessor :: Client -> Message -> IO Client
messageProcessor client (Message _ "PONG" _) = return client
messageProcessor client (Message _ "PING" (server1:_)) = do
    sendClient client $ ":lambdircd PONG lambdircd :" ++ server1
    return client
messageProcessor client (Message _ "NICK" (nick:_)) = return client {IRCD.nick = Just nick}
messageProcessor client (Message _ "USER" (user:_:_:[realname])) =
    return client {IRCD.user = Just user, realName = Just realname}
messageProcessor client (Message _ command _) = do
    sendClient client $ ":lambdircd 421 " ++ nick ++ (' ':command) ++ " :Unknown command"
    return client
  where nick = fromJust $ IRCD.nick client
