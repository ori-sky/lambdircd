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

import IRC.Server as IRCD
import IRC.Server.Options
import IRC.Server.Environment

main :: IO ()
main = serveIRC defaultEnv
    { options = defaultOptions
        { plugins =
            [ "Pong"
            , "Nick"
            , "User"
            ]
        , pingTimeout = 5
        }
    }

{-
processMessage _ client (Message _ "PING"(server1:_)) = do
    sendClient client $ ":lambdircd PONG lambdircd :" ++ server1
    return client

processMessage _ client (Message _ command _) = do
    sendClient client $ ":lambdircd 421 " ++ nick' ++ (' ':command) ++ " :Unknown command"
    return client
  where nick' = fromMaybe "*" (Client.nick client)
-}
