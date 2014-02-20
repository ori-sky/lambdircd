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

module Plugin.Load where

import System.Plugins.Load
import qualified Plugin.API as API

loadPlugin :: String -> IO (Maybe API.Interface)
loadPlugin name = do
    p <- pdynload ("plugins/"++name++".o") ["."] [] "Plugin.API.Interface" "plugin"
    case p of
        LoadSuccess _ a -> return $ Just $ case API.name a of
            ""  -> a {API.name=name}
            _   -> a
        LoadFailure e   -> mapM_ putStrLn e >> return Nothing
