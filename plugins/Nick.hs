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

module Nick (plugin) where

import qualified Data.Map as M
import Control.Monad.State
import IRCD.Types
import IRCD.Env
import IRCD.Clients

plugin :: Plugin
plugin = defaultPlugin {handlers=[CommandHandler "NICK" nickHandler]}

nickHandler :: HandlerSpec
nickHandler (ClientSrc client) (Message _ _ _ (nick':_)) = do
    nicks <- gets (byNick . envClients)
    if nick' `M.member` nicks
        then return [GenericAction $ liftIO (putStrLn "Nickname is already in use")]
        else do
            modify $ mapEnvClients (replaceClient client client {nick=Just nick'})
            return [GenericAction $ liftIO (putStrLn ("NICK " ++ nick'))]
nickHandler (ClientSrc client) _ = return [GenericAction $ liftIO (putStrLn "No nickname given")]
