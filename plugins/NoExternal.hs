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

module NoExternal (plugin) where

import IRCD.Types.Plugin
import IRCD.Types.Server

plugin :: Plugin
plugin = defaultPlugin {transformers=[Transformer noExt 50]}

noExt :: TransformerSpec
noExt action@(PrivmsgAction (ClientSrc client) (ChannelDst channel) msg io)
    | 'n' `elem` modes channel && channel `notElem` channels client = return []
    | otherwise = return [action]
noExt action = return [action]
