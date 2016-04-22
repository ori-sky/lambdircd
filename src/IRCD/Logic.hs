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

module IRCD.Logic (doLogic) where

import Control.Monad.State
import Hoist
import IRCD.Types
import IRCD.Message

doLogic :: Client -> String -> StateT Env IO ()
doLogic client line = do
    actions <- gets envHandlers >>= hoistState . mapM fh >>= return . concat
    ts <- gets envTransformers
    actions' <- mapM (ft ts ts) actions >>= return . concat
    liftIO (print actions')
    --mapM_ actionSpec as
    --hoistState (mapM (ft ts ts) actions) >>= mapM_ actionSpec . concat
  where msg = parseMessage line
        fh (GenericHandler spec) = spec (ClientSrc client) msg
        fh (CommandHandler cmd spec)
            | cmd == command msg = spec (ClientSrc client) msg
            | otherwise = return []

        ft ts [] action = return [action]
        ft ts (Transformer spec _ : xs) action = hoistState (spec action) >>= \case
            (False, actions) -> mapM (ft ts ts) actions >>= return . concat
            (True, actions)  -> do
                this <- ft ts xs action 
                mapM_ actionSpec this
                rest <- mapM (ft ts ts) actions >>= return . concat
                return (concat [this, rest])

doLogic :: Client -> String -> StateT Env IO ()
doLogic client line = do
    actions <- gets envHandlers >>= hoistState . mapM fh >>= return . concat
    ts <- gets envTransformers
    map (transformAction ts) actions
  where transformAction (Transformer spec _ : ts) action = hoistState (spec action)\
