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

{-# LANGUAGE RankNTypes #-}

module Plugin where

import IRC.Message (Message)
import qualified IRC.Server.Channel as Chan
import IRC.Server.Environment (Env)

type CommandHSpec   = Env -> Message -> Env
type TransformHSpec = Env -> Env

data Handler =
    CommandHandler String CommandHSpec
  | TransformHandler TransformHSpec

data Interface = Interface
    { isPlugin  :: Bool
    , name      :: String
    , handlers  :: [Handler]
    , cModes    :: [Chan.Mode]
    }

defaultPlugin = Interface
    { isPlugin  = True
    , name      = ""
    , handlers  = []
    , cModes    = []
    }
