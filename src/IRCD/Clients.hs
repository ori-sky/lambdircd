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

module IRCD.Clients (insertClient) where

import qualified Data.Map as M (insert)
import qualified Data.IntMap as IM (insert)
import IRCD.Types.Client
import IRCD.Types.Clients

insertClient :: Client -> Clients -> Clients
insertClient client clients = clients
    { byUid  = byUid'
    , byNick = byNick'
    }
  where
    byUid' = case uid client of
        Nothing   -> byUid clients
        Just uid' -> IM.insert uid' client (byUid clients)
    byNick' = case nick client of
        Nothing    -> byNick clients
        Just nick' -> M.insert nick' client (byNick clients)
