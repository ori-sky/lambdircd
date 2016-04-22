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

module Core.Welcome (plugin) where

import IRCD.Types
import IRCD.Helper

plugin :: Plugin
plugin = defaultPlugin {transformers=[Transformer welcome 80]}

welcome :: TransformerSpec
welcome action@(RegisterAction src@(ClientSrc client) _) = return (True, [GenericAction io])
  where io = reply_ src "Welcome!"
welcome _ = return (True, [])