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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Plugin where

import IRC.Message (Message)
import IRC.Server.Environment (Env)

--type CommandHandler = Env.Env -> Message -> IO Env.Env

type CommandHSpec   = Env -> Message -> Env
type CModeHSpec     = Env -> Env

instance Show CommandHSpec where show _ = "<function>"
instance Show CModeHSpec where show _ = "<function>"

data Handler =
    CommandHandler String CommandHSpec
  | CModeHandler Char CModeHSpec
    deriving (Show)

data Interface = Interface
    { isPlugin  :: Bool
    , name      :: String
    , handlers  :: [Handler]
    }

defaultPlugin = Interface
    { isPlugin  = True
    , name      = ""
    , handlers  = []
    }
