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

module IRC.Server.Channel.Helper where

import qualified Data.IntMap as IM
import Control.Monad (unless)
import IRC.Server.Client (clientToMask, sendClient)
import qualified IRC.Server.Client as Client
import IRC.Server.Channel
import qualified IRC.Server.Environment as Env

sendChannelOthers :: Env.Env -> Channel -> String -> IO ()
sendChannelOthers env chan msg = mapM_ f (uids chan)
  where
    local = Env.local env
    client = Env.client env
    Just uid = Client.uid client
    f u = unless (u == uid) $ sendClient c msg
      where c = Env.clients local IM.! u

sendChannelOthersFrom :: String -> Env.Env -> Channel -> String -> IO ()
sendChannelOthersFrom src env chan msg = sendChannelOthers env chan newMsg
  where newMsg = ':' : src ++ ' ' : msg

sendChannelOthersFromClient :: Client.Client -> Env.Env -> Channel -> String -> IO ()
sendChannelOthersFromClient cli = sendChannelOthersFrom $ show (clientToMask cli)
