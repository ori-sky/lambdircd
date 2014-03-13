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

module IRC.Server.Environment where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Concurrent.MVar
import IRC.Action
import IRC.Message (Message)
import IRC.Server.Client (Client, defaultClient)
import IRC.Server.Client.Helper
import qualified IRC.Server.Channel as Chan
import Data.ConfigFile.Monadic

data Shared = Shared
    { clients   :: IM.IntMap Client
    , uids      :: M.Map String Int
    , channels  :: M.Map String Chan.Channel
    }

defaultShared :: Shared
defaultShared = Shared
    { clients   = IM.empty
    , uids      = M.empty
    , channels  = M.empty
    }

data Env = Env
    { config            :: ConfigParser
    , client            :: Client
    , commandHandlers   :: M.Map String (Env -> Message -> Env)
    , cModeHandlers     :: M.Map Char (Env -> Env)
    , shared            :: Maybe (MVar Shared)
    , local             :: Shared
    , actions           :: [Action]
    }

defaultEnv :: Env
defaultEnv = Env
    { config            = emptyCP
    , client            = defaultClient
    , commandHandlers   = M.empty
    , cModeHandlers     = M.empty
    , shared            = Nothing
    , local             = defaultShared
    , actions           = []
    }

whenRegistered :: Env -> Env -> Env
whenRegistered env = whenClientRegistered cli env
  where cli = client env
