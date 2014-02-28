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
import Control.Concurrent.STM
import IRC.Message (Message(..))
import IRC.Server.Client
import qualified IRC.Server.Options as Opts

data Shared = Shared
    { clients   :: M.Map Integer Client
    }

defaultShared :: Shared
defaultShared = Shared
    { clients   = M.empty
    }

data Env = Env
    { options   :: Opts.Options
    , client    :: Client
    , handlers  :: M.Map String (Env -> Message -> IO Env)
    , shared    :: Maybe (TVar Shared)
    }

defaultEnv :: Env
defaultEnv = Env
    { options   = Opts.defaultOptions
    , client    = defaultClient
    , handlers  = M.empty
    , shared    = Nothing
    }
