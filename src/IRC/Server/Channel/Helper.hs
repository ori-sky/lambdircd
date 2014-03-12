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

import Data.List (nub)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad (unless, forM_)
import IRC.Server.Client.Helper
import qualified IRC.Server.Client as Cli
import IRC.Server.Channel
import qualified IRC.Server.Environment as Env

sendChannel :: Env.Env -> Channel -> String -> IO ()
sendChannel env chan msg = mapM_ f (uids chan)
  where
    local = Env.local env
    f u = sendClient c msg
      where c = Env.clients local IM.! u

sendChannelFrom :: String -> Env.Env -> Channel -> String -> IO ()
sendChannelFrom src env chan msg = sendChannel env chan newMsg
  where newMsg = ':' : src ++ ' ' : msg

sendChannelFromClient :: Cli.Client -> Env.Env -> Channel -> String -> IO ()
sendChannelFromClient cli = sendChannelFrom $ show (clientToMask cli)

sendChannelOthers :: Env.Env -> Channel -> String -> IO ()
sendChannelOthers env chan msg = mapM_ f (uids chan)
  where
    local = Env.local env
    client = Env.client env
    Just uid = Cli.uid client
    f u = unless (u == uid) $ sendClient c msg
      where c = Env.clients local IM.! u

sendChannelOthersFrom :: String -> Env.Env -> Channel -> String -> IO ()
sendChannelOthersFrom src env chan msg = sendChannelOthers env chan newMsg
  where newMsg = ':' : src ++ ' ' : msg

sendChannelOthersFromClient :: Cli.Client -> Env.Env -> Channel -> String -> IO ()
sendChannelOthersFromClient cli = sendChannelOthersFrom $ show (clientToMask cli)

getUniqCommon :: Env.Env -> Cli.Client -> [Int]
getUniqCommon env cli = nub $ concatMap uids $ map (locChans M.!) cliChans
  where
    local = Env.local env
    locChans = Env.channels local
    cliChans = Cli.channels cli

sendUniqCommon :: Env.Env -> Cli.Client -> String -> IO ()
sendUniqCommon env cli msg = forM_ (map (locClients IM.!) uniqUids) $ flip sendClient msg
  where
    local = Env.local env
    locClients = Env.clients local
    uniqUids = getUniqCommon env cli
