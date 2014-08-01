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

module IRCD.Types.Plugin where

import Control.Monad.State
import IRCD.Types.Action
import IRCD.Types.Message (Message)
import IRCD.Types.Server hiding (name)

type HandlerSpec = Source -> Message -> State Env [Action]
type TransformerSpec = Action -> State Env [Action]

data Handler = GenericHandler HandlerSpec
             | CommandHandler String HandlerSpec

data Transformer = Transformer TransformerSpec Int

defaultTransformer :: TransformerSpec -> Transformer
defaultTransformer f = Transformer f 100

data Plugin = Plugin
    { name          :: String
    , handlers      :: [Handler]
    , transformers  :: [Transformer]
    }

defaultPlugin :: Plugin
defaultPlugin = Plugin
    { name          = ""
    , handlers      = []
    , transformers  = []
    }
