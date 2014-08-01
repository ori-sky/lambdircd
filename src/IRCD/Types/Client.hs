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

module IRCD.Types.Client where

import System.IO (Handle)
import IRCD.Types.Channel (Channel)

data Client = Client
    { uid           :: Maybe Int
    , handle        :: Maybe Handle
    , registered    :: Bool
    , nick          :: Maybe String
    , user          :: Maybe String
    , realName      :: Maybe String
    , host          :: Maybe String
    , channels      :: [Channel]
    } deriving (Show)

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
