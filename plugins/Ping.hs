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

module Ping (plugin) where

import Control.Monad.State (liftIO)
import IRCD.Types.Plugin
import IRCD.Types.Action
import IRCD.Types.Message

plugin :: Plugin
plugin = defaultPlugin {handlers=[CommandHandler "PING" ping]}

ping :: HandlerSpec
ping (Message src tags prefix cmd (server1:_)) = return [GenericAction io]
  where io = liftIO $ putStrLn "received PING"
ping (Message src _ _ _ _) = return [GenericAction io]
  where io = liftIO $ putStrLn "not enough parameters for PING"
