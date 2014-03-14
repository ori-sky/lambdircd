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

module Config where

import Data.Either.Utils
import Data.ConfigFile.Monadic
import Control.Monad
import Control.Monad.Error

defaultCP :: ConfigParser
defaultCP = forceEither $ return emptyCP
    >>= add_section "info"
    >>= set "info"      "name"          "lambdircd"
    >>= set "info"      "network"       "LambdaNet"
    >>= set "info"      "description"   "A lambdircd server"
    >>= add_section "listen"
    >>= set "listen"    "port"  "6667"
    >>= set "listen"    "queue" "5"
    >>= set "listen"    "defer" "30"
    >>= add_section "client"
    >>= set "client"    "connect_timeout"   "20"
    >>= set "client"    "ping_timeout"      "240"
    >>= set "client"    "max_channels"      "5"
    >>= add_section "plugins"
    >>= set "plugins"   "load"  ""

loadConfig :: String -> IO ConfigParser
loadConfig path = do
    eitherCP <- runErrorT $ join $ liftIO $ readfile path defaultCP
    case eitherCP of
        Left e      -> print e >> return defaultCP
        Right cp    -> return cp

getConfig' cp sec = forceEither . get cp sec

getConfigString :: ConfigParser -> SectionSpec -> OptionSpec -> String
getConfigString = getConfig'
getConfigBool :: ConfigParser -> SectionSpec -> OptionSpec -> Bool
getConfigBool = getConfig'
getConfigInt :: ConfigParser -> SectionSpec -> OptionSpec -> Int
getConfigInt = getConfig'
