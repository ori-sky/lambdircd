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

module IRCD.Helper where

import qualified Data.IntMap as IM
import Control.Monad.State
import System.IO (hPutStrLn)
import IRCD.Types
import IRCD.Env
import IRCD.Clients

updateClient :: (Client -> Client) -> Int -> State Env ()
updateClient f uid' = do
    uids <- gets (byUid . envClients)
    modify $ mapEnvClients $ case uid' `IM.lookup` uids of
        Nothing -> insertClient (f (defaultClient uid'))
        Just client'  -> replaceClient client' (f client')

updateClientNick :: String -> Int -> State Env ()
updateClientNick nick' = updateClient (\c -> c {nick=Just nick'})

updateClientUser :: String -> Int -> State Env ()
updateClientUser user' = updateClient (\c -> c {user=Just user'})

updateClientRealName :: String -> Int -> State Env ()
updateClientRealName realname = updateClient (\c -> c {realName=Just realname})

updateClientRegistered :: Bool -> Int -> State Env ()
updateClientRegistered r = updateClient (\c -> c {registered=r})

reply_ :: Source -> String -> StateT Env IO ()
reply_ (ClientSrc client) msg = case handle client of
    Nothing -> return ()
    Just h -> liftIO (hPutStrLn h msg)
