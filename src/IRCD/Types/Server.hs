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

module IRCD.Types.Server where

import qualified Data.Map as M (Map, empty)
import qualified Data.IntMap as IM (IntMap, empty)
import System.IO (Handle)

data Client = Client
    { uid           :: Maybe Int
    , handle        :: Maybe Handle
    , registered    :: Bool
    , nick          :: Maybe String
    , user          :: Maybe String
    , realName      :: Maybe String
    , host          :: Maybe String
    , channels      :: [Channel]
    } deriving (Show, Eq)

data Clients = Clients
    { byUid     :: IM.IntMap Client
    , byNick    :: M.Map String Client
    } deriving Show

data Channel = Channel
    { name      :: String
    , modes     :: [Char]
    , clients   :: [Client]
    } deriving (Show, Eq)

data Source = ClientSrc Client
data Destination = ChannelDst Channel

data Env = Env
    { envClients :: Clients
    } deriving Show

defaultClient :: Client
defaultClient = Client
    { uid           = Nothing
    , handle        = Nothing
    , registered    = False
    , nick          = Nothing
    , user          = Nothing
    , realName      = Nothing
    , host          = Nothing
    , channels      = []
    }

defaultClients :: Clients
defaultClients = Clients
    { byUid     = IM.empty
    , byNick    = M.empty
    }

defaultEnv :: Env
defaultEnv = Env
    { envClients = defaultClients
    }
