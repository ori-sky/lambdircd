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

import Control.Monad.State
import System.IO (hPutStrLn)
import IRCD.Types

updateClient :: Client -> Client -> State Env ()
updateClient old new = undefined

mapClient :: (Client -> Client) -> Client -> State Env ()
mapClient f x = updateClient x (f x)

reply_ :: Source -> String -> StateT Env IO ()
reply_ (ClientSrc client) msg = case handle client of
    Nothing -> return ()
    Just h -> liftIO (hPutStrLn h msg)
