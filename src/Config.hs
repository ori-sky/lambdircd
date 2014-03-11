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

{-
data Config = Config
    { plugins           :: [String]
    , port              :: Int
    , connectTimeout    :: Int
    , pingTimeout       :: Int
    , maxChannels       :: Int
    }
-}

defaultCP :: ConfigParser
defaultCP = forceEither $ return emptyCP
    >>= s "plugins"             ""
    >>= s "port"                "6667"
    >>= s "connect_timeout"     "30"
    >>= s "ping_timeout"        "240"
    >>= s "max_channels"        "5"
  where s = set "DEFAULT"

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
