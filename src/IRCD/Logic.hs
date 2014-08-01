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

module IRCD.Logic (doLogic) where

import qualified Data.IntMap as IM (toList)
import Control.Monad.State
import System.IO (hPutStrLn)
import qualified IRCD.TS6 as TS6
import IRCD.Types.Server

doLogic :: Client -> String -> StateT Env IO ()
doLogic client line = do
    handles' <- gets (byUid . envClients) >>= return . map (handle . snd) . IM.toList
    liftIO (mapM_ f handles')
  where f Nothing = return ()
        f (Just handle') = hPutStrLn handle' $ "[::" ++ uidString ++ "] " ++ line
        uidString = maybe "*" TS6.intToID (uid client)
