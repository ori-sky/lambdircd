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

module Who where

import IRC.Message
import IRC.Numeric
import IRC.Server.Environment (whenRegistered)
--import qualified IRC.Server.Environment as Env
import Plugin

plugin = defaultPlugin {handlers=[("WHO", who)]}

who :: CommandHandler
who env (Message _ _ ("0":_)) = whenRegistered env $ do
    sendNumeric env numRPL_ENDOFWHO ["*", "End of /WHO list"]
    return env
who env (Message _ _ (name:_)) = whenRegistered env $ do
    sendNumeric env numRPL_ENDOFWHO [name, "End of /WHO list"]
    return env
who env _ = whenRegistered env $ do
    sendNumeric env numERR_NEEDMOREPARAMS ["WHOIS", "Not enough parameters"]
    return env
