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
main = serveIRC defaultOptions handleMessage

handleMessage :: MessageHandler
handleMessage opts client message = do
    putStrLn $ show message
    processMessage opts client message

processMessage :: MessageHandler

processMessage _ client (Message _ "NICK" (nick:_)) = return client {IRCD.nick=Just nick}
processMessage _ client (Message _ "NICK" _) = do
    sendClient client $ ":lambdircd 431 " ++ nick' ++ " :No nickname given"
    return client
  where nick' = fromMaybe "*" (IRCD.nick client)

processMessage _ client (Message _ "USER" (user:_:_:realname:_)) =
    return client {IRCD.user=Just user, realName=Just realname}
processMessage _ client (Message _ "USER" _) = do
    sendClient client $ ":lambdircd 461 " ++ nick' ++ " USER :Not enough parameters"
    return client
  where nick' = fromMaybe "*" (IRCD.nick client)

processMessage _ client (Message _ "PONG" _) = return client
processMessage _ client (Message _ "PING"(server1:_)) = do
    sendClient client $ ":lambdircd PONG lambdircd :" ++ server1
    return client

processMessage _ client (Message _ command _) = do
    sendClient client $ ":lambdircd 421 " ++ nick' ++ (' ':command) ++ " :Unknown command"
    return client
  where nick' = fromMaybe "*" (IRCD.nick client)
