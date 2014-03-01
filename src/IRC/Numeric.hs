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

module IRC.Numeric where

import Data.Word (Word16)
import Data.Maybe (fromMaybe)
import IRC.Server.Environment (Env)
import qualified IRC.Server.Environment as Env (client)
import IRC.Server.Client (sendClient)
import qualified IRC.Server.Client as Client (nick)

data Numeric = Numeric Word16

instance Show Numeric where
    show (Numeric num) = replicate numZeros '0' ++ numStr
      where
        numStr = show num
        numZeros = maximum [0, 3 - length numStr]

sendNumeric :: Env -> Numeric -> [String] -> IO ()
sendNumeric env numeric params = sendClient client $
    unwords $ [":lambdircd", show numeric, nick] ++ init params ++ [':' : last params]
  where
    client = Env.client env
    nick = fromMaybe "*" (Client.nick client)
