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

{-# LANGUAGE LambdaCase #-}

module IRCD.Plugin.Load where

import System.Plugins.Load
import IRCD.Types.Plugin

loadPlugin :: String -> IO (Maybe Plugin)
loadPlugin name' = load_ ("plugins/" ++ name' ++ ".o") ["src"] "plugin" >>= \case
    LoadSuccess _ plugin -> return $ Just $ case name plugin of
        "" -> plugin {name=name'}
        _  -> plugin
    LoadFailure e -> mapM_ putStrLn e >> return Nothing
