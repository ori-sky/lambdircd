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

module User (plugin) where

import IRCD.Types
import IRCD.Helper
import Hoist

plugin :: Plugin
plugin = defaultPlugin {handlers=[CommandHandler "USER" userHandler]}

userHandler :: HandlerSpec
userHandler src@(ClientSrc client) (Message _ _ _ (user':_:_:realname:_))
    | registered client = return [GenericAction $ reply_ src "You may not reregister"]
    | otherwise = return [ UserChangeAction src (user client) user' ioUser
                         , RealNameChangeAction src (realName client) realname ioRealName
                         ]
  where ioUser = hoistState $ updateClientUser user' (uid client)
        ioRealName = hoistState $ updateClientRealName realname (uid client)
userHandler src _ = return [GenericAction $ reply_ src "Not enough parameters"]
