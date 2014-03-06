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
import IRC.Server.Client (sendClient)
import qualified IRC.Server.Environment as Env (client)
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

numRPL_WELCOME              = Numeric 001
numRPL_YOURHOST             = Numeric 002
numRPL_CREATED              = Numeric 003
numRPL_WHOISUSER            = Numeric 311
numRPL_ENDOFWHOIS           = Numeric 318
numRPL_NAMREPLY             = Numeric 353
numRPL_ENDOFNAMES           = Numeric 366
numRPL_MOTD                 = Numeric 372
numRPL_MOTDSTART            = Numeric 375
numRPL_ENDOFMOTD            = Numeric 376
numERR_NOSUCHNICK           = Numeric 401
numERR_NOSUCHCHANNEL        = Numeric 403
numERR_NORECIPIENT          = Numeric 411
numERR_NOTEXTTOSEND         = Numeric 412
numERR_UNKNOWNCOMMAND       = Numeric 421
numERR_NONICKNAMEGIVEN      = Numeric 431
numERR_NICKNAMEINUSE        = Numeric 433
numERR_NICKCOLLISION        = Numeric 436
numERR_USERONCHANNEL        = Numeric 443
numERR_NEEDMOREPARAMS       = Numeric 461
numERR_ALREADYREGISTERED    = Numeric 462
